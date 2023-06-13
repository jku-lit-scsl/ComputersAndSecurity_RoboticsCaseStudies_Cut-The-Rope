# adapted version of original cut the rope: condense all targets (if there are more than one)
# into a single target node, and play "cut the rope" without change (except for bugfixes as annotated below)
library(HyRiM)

# read the attack graph of interest to create the variables
# routes: a list of attack paths
# target: the final node in the attack graph

################################################################################
# externally supplied (i.e., elsewhere defined) variables
# these are defined as part of the individual experiments:
# + node_order: ordering of nodes (can be a topological order or any other)
# + randomSteps: a function taking a route for the adversary, and returning a 
#   probability mass function giving the likelihoods of how far the attacker may have come on the route
# + attackRateList: list of attack rates to iterate over, whose values are used by "randomSteps"
# + defenseRateList: list of defense rates (also used internally by "randomSteps" if this uses a geometric distribution)

# what follows from here onwards is the same for all experiments
################################################################################
# preparation of the attack graph: 
# add virtual entry node if there are multiple possible entry points
roots <- V(attack_graph)[degree(attack_graph, mode="in")==0] %>% as_ids
k <- length(roots)
if (k > 1) {
  # add a virtual starting point
  entry <- "attacker_entry_node"  # the virtual node
  attack_graph <- add_vertices(attack_graph, 1, name = entry)
  edgelist <- rep(entry, times=2*k)
  edgelist[2*(1:k)] <- roots
  attack_graph <- add_edges(attack_graph, edgelist)
  attack_graph <- set_edge_attr(attack_graph,
                                name = "weight",
                                index = get.edge.ids(attack_graph, edgelist),
                                value = 1)   # the virtual start is no obstacle towards the real entry point
} else {
  entry <- roots[1]
}

################################################################################
# condense all attack targets into a single target node
target_list <- V(attack_graph)[degree(attack_graph, mode="out")==0] %>% as_ids
vertexNo <- matrix(0, nrow = 1, ncol = gorder(attack_graph))
colnames(vertexNo) <- get.vertex.attribute(attack_graph, "name")
jointVertex <- gorder(attack_graph) - length(target_list) + 1
vertexNo[,target_list] <- jointVertex
vertexNo[vertexNo == 0] <- 1:(jointVertex - 1)
attack_graph <- contract.vertices(attack_graph, mapping = vertexNo)
# the "vertexNo" may be saved for later, to recover the original node
# numbers before the renaming done by "contract.vertices"

# the noder order must be re-computed after this renaming
node_order <- as_ids(topo_sort(attack_graph))  # determine the node order from a topological sort...
# note that the topological sort will put the final (single) target node at the end of the resulting list
# we will use this below, when we output the chances to reach this (last) node, as the performance measure
# of the optimized security policy
target_list <- V(attack_graph)[degree(attack_graph, mode="out")==0] %>% as_ids
################################################################################


################################################################################
# game setup: for each target, enumerate all paths => attack strategies,
# and enumerate all points where the defender can become active

routes <- lapply(all_simple_paths(attack_graph, from=entry, to=target_list), as_ids)
V <- unique(unlist(routes)) # get all nodes from all routes
node_order <- intersect(node_order,  # use the pre-defined node_order for the given experiment...
                        V) # ...and retain only those nodes that are also potential adversarial starting points

# the defender can check everywhere, except on nodes that we exclude to avoid trivialities
# unless we have an externally restricted action space for the defender, the default is to defend everywhere
if (!exists("as1")) {
  as1 <- setdiff(V, c("attacker_entry_node", roots, target_list))
}

as2 <- routes  # action space for the attacker
m <- length(as2)
n <- length(as1)

# assume one hypothetical adversary for each possible start location
advList <- setdiff(V, c(entry, target_list))  # exclude trivial cases where the attacker starts right from the finish line (=> would not make sense)
# we assign equal contributions to the defender's loss caused by all hypothetical adversaries
Theta <- rep(1/n, times = length(advList))
names(Theta) <- advList

# avoid the loops being skipped if the experiments do not define attack or defense rates to try
if (!exists("defenseRateList")) { defenseRateList <- 0 }
if (!exists("attackRateList")) { attackRateList <- 0 }

for(defenseRate in defenseRateList){
  for(attackRate in attackRateList) {
    cat("\n++++++++++++++++++++++++++++++++\nattack rate = ", attackRate, ", defense rate = ", defenseRate, "\n")
    payoffsList <- NULL  # to collect all payoffs for the multi-criteria game
    
    # this loop will only take a single iteration (we have only one target in the current version; multiple targets are "theoretically possible", and up to future work/studies)    
    for(target in target_list) {  # each target is its own goal for the defender to optimize against all adversary avatars
     
      payoffMatrix <- list() # to take up the utility distributions
      for(i in as1) { # run over all spots to inspect
        for(path in as2) { # run over all attack paths
          U <- rep(0, length(V))
          names(U) <- V
          
          for(avatar in advList) {
            
            L <- rep(0, length(V))
            names(L) <- V   # for indexing by node ID
            # adversary moves only if it is on this path
            if (avatar %in% path) {
              route <- path[which(path == avatar):length(path)]
              
              # let the adv. take a random number of steps
              pdfD <- randomSteps(route, attackRate, defenseRate)
              
              # correction over the past (2019) version of the code to avoid the cutpoint returning as -1
              cutPoint <- min(which(route == i), length(route)) 
              
              # truncate the distribution; there is the special case of the avatar
              # able to take the first step with probability 100%, but the defender
              # blocking just at this point. In that case, the adversary would not move
              # move as far as it can, and stop at the cutpoint
              if (sum(pdfD[1:cutPoint]) == 0) {
                payoffDistr <- rep(0, cutPoint)
                payoffDistr[cutPoint] <- 1  # adversary moves exactly to the cutpoint
              } else {
                # adversary moves at random
                payoffDistr <- pdfD[1:cutPoint]/sum(pdfD[1:cutPoint])
              }
              
              L[route[1:cutPoint]] <- payoffDistr
            }
            else { # otherwise, the adversary doesn't move
              # note that this bit of code expresses that the full mass is here at the starting location 
              # of the avatar, implying that there is zero mass on any of the goals
              # => this is consistent with the situation that the attack 'does not happen' over this route at all
              L[avatar] <- 1  
              # Remark: without the above line, the losses would come up with empty categories (which the solver cannot handle)
            }
            # update the mix over all adversarial avatars
            U <- U + Theta[avatar] * L
          }
          # construct the loss distribution
          U <- U / sum(U)  # avoid warnings (by lossDistribution) to not have normalized yet
          
          # re-order according to shortest distances to make the tail masses = the probabilities
          # to hit (one of) the target(s)
          # the variable "node_order" is supplied externally
          U <- U[node_order]
          
          # fix zero-probability categories, if a path does not put mass there
          # since the adversary does not arrive at this (particular) goal.
          # this is just to avoid the later solver to throw an exception, 
          # and we will keep the "noise" below the tolerance threshold (1e-5); see below
          U[U == 0] <- 1e-7
          
          ld <- lossDistribution(U, discrete=TRUE, dataType="pdf", supp=c(1,length(V)), smoothing="always", bw = 0.2)
          payoffMatrix <- append(payoffMatrix, list(ld))
        } #loop over all attack paths
      } #loop over all spot check locations
      
      payoffsList <- append(payoffsList, payoffMatrix)
    }   #loop over all target nodes (if there is more than one; not yet implemented/studied)
    # construct and solve the game
    
    G <- mosg(n, m, goals = length(target_list), 
              losses = payoffsList, byrow = TRUE, 
              defensesDescr = (as1 %>% as.character))
    eq <- mgss(G, tol=1e-5)  # compute a multi-goal security strategy
    print(eq)      # printout the equilibrium (optimal defense)
    # print out the assurance, i.e., optimal likelihood to hit the goal
    # by the node ordering, the target is the *last* entry in the optimally assured loss distribution
    # (i.e., the highest category of damage)
    loc <- eq$assurances$`1`$dpdf
    print(round(loc[length(loc)], digits=3))
    
  } # loop over all attack rates
} # loop over all defense rates

