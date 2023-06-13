# define the attack graph and compile it for an application of
# "Cut-The-Rope"

# Example implementing the attack graph
# "Physical Attack Tree" on page 20 of the Alias' report

library(igraph)

attack_graph <- graph_from_literal(
  1 -+ 2,
  2 -+ 3:4:5,
  3 -+ 6,
  4 -+ 6,
  5 -+ 7 -+ 8 -+ 9
)

node_order <- as_ids(topo_sort(attack_graph))  # determine the node order from a topological sort...

# in absence of better knowledge, we count each edge with the same "length"
set_edge_attr(attack_graph, name = "weight", value = 1)

# # uncomment these lines, if the experiment should run with only partial knowledge
# # about the attack graph. The "known_fraction" is any value between 0 and 1, reflecting
# # how many nodes are included in the defendable set
# # the "set.seed" is required to have the *same* sample between the heuristic and CTR executions,
# # to compare performances on the same (reduced) defense sets. Uncomment this line for an individual
# # run of the experiment (not for comparative reasons)
# set.seed(seed = 3141592)  # the seed is arbitrary (here, the it is "Pi" without decimal dot :-))
# known_fraction <- 0.75 # we know x% of the graph
# as1 <- c(1,2,3,4,5,7,8) # exclude nodes 6 and 9, since these are the final targets
# as1 <- sample(as1, round(known_fraction * length(as1), digits = 0))  # exclude the target nodes manually here
# cat(c("reduced defense set = ", as1))