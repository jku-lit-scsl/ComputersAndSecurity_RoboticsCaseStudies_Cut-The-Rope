# experiment 3_H:
# conducted: 2022-01-24
# setup:
# - adversary: takes the shortest/easiest route
# - defender: spotchecks locations uniformly at random and periodically
# - attack graph: MIR100

rm(list = ls())  # clean up the workspace to avoid side-effects

outputLogFile <- "experiment_3_H.log"

con <- file(outputLogFile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
print(outputLogFile)
print(date())

source(file = "attack_graph_MIR100.R")

attackRateList <- 2  # parameter lambda

# steps determined by hardness to exploit
randomSteps <- function(route, attackRate = NULL, defenseRate = NULL) {
  hardness <- edge_attr(attack_graph, "edge_probabilities", E(attack_graph, path=route))
  hardness[is.na(hardness)] <- 1 # fix missing hardness values: if we know nothing, we consider the edge easy (trivial) to traverse
  pdfD <- c(1 - hardness, 1) * c(1, cumprod(hardness))
  pdfD <- pdfD / sum(pdfD)
  return(pdfD)
}

source(file = "heuristic_defense.R")

sink()
file.show(outputLogFile)
