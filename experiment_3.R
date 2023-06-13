# experiment 3:
# conducted: 2022-01-24
# setup:
# - adversary: moves at Poisson rate
# - defender: periodic 
# - attack graph: MIR100

rm(list = ls())  # clean up the workspace to avoid side-effects

outputLogFile <- "experiment_3.log"

con <- file(outputLogFile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
print(outputLogFile)
print(date())

source(file = "attack_graph_MIR100.R")

# steps determined by hardness to exploit
randomSteps <- function(route, attackRate = NULL, defenseRate = NULL) {
  hardness <- edge_attr(attack_graph, "edge_probabilities", E(attack_graph, path=route))
  hardness[is.na(hardness)] <- 1 # fix missing hardness values: if we know nothing, we consider the edge easy (trivial) to traverse
  pdfD <- c(1 - hardness, 1) * c(1, cumprod(hardness))
  pdfD <- pdfD / sum(pdfD)
  return(pdfD)
}

source("ctr-core_1.R")

sink()
file.show(outputLogFile)
