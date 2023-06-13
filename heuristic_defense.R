# version of the experiment with a common-sense baseline heuristic:
# Defender guards "everywhere", versus attacker, taking the shortest route
# towards its goal
library(HyRiM)

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

target <- target_list[1] # there is only one target

starting_points <- setdiff(V, target_list) # let the attacker start from anywhere, except the goal (to avoid trivialities)

for(defenseRate in defenseRateList){
  for(attackRate in attackRateList) {
    
    U <- rep(0, length(V))
    names(U) <- V
    
    for(i in as1) { # run over all spots to inspect
      for(j in starting_points) { # run over all possible starting points
        # the "weight" property must have been defined in the experiment setups accordingly
        route <- unlist(lapply(shortest_paths(attack_graph, from = j, to = target)$vpath, as_ids))
        pdfD <- randomSteps(route, attackRate, defenseRate)
        
        cutPoint <- min(which(route == i),length(route))
        
        if (sum(pdfD[1:cutPoint]) == 0) {
          payoffDistr <- rep(0, cutPoint)
          payoffDistr[cutPoint] <- 1  # adversary moves exactly to the cutpoint
        } else {
          # adversary moves at random
          payoffDistr <- pdfD[1:cutPoint]/sum(pdfD[1:cutPoint])
        }
        L <- rep(0, length(V))
        names(L) <- V   # for indexing by node ID
        L[route[1:cutPoint]] <- payoffDistr
        
        # law of total probability: L = conditional likelihood (route = j, cut at i)
        # 1/|AS1| ... uniform distribution over defense nodes
        # 1/|V| ... uniform distribution over starting points
        U <- U + L * (1/length(as1)) * (1/length(V)) 
      }
    }
    # output the probability to hit the target
    cat("defenseRate = ", defenseRate, ", attack rate = ", attackRate, ", hitting the target ", target, " with chance ", U[target], "\n\n")
  } # loop over all attack rates
} # loop over all defense rates

