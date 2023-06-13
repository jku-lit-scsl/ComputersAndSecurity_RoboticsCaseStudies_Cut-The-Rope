# experiment 4_H:
# conducted: 2022-01-24
# setup:
# - adversary: takes the shortest/easiest route
# - defender: spotchecks locations uniformly at random and in exponentially distributed time intervals
# - attack graph: MIR100

rm(list = ls())  # clean up the workspace to avoid side-effects

outputLogFile <- "experiment_4_H.log"

con <- file(outputLogFile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
print(outputLogFile)
print(date())

source(file = "attack_graph_MIR100.R")

# let the defender move slower (lamdba=1), equally fast (lambda=2) or faster (lambda=3)
defenseRateList <- c(1,2,3) # parameter lambda_D

# random steps determined by hardness, within a random time frame dictated by the defender's idleness periods
randomSteps <- function(route, attackRate = NULL, defenseRate = NULL) {
  hardness <- edge_attr(attack_graph, "edge_probabilities", E(attack_graph, path=route))
  hardness[is.na(hardness)] <- 1 # fix missing hardness values: if we know nothing, we consider the edge easy (trivial) to traverse
  
  # determine the attack rate depending on the path  
  geomean <- function(x) {
    return(exp(mean(log(x[x>0]))));
  }
  attackRate <- 1 / geomean(hardness)
  
  pdfD <- dgeom(x = 0:(length(route) - 1), prob = attackRate / (attackRate + defenseRate))
  pdfD <- pdfD / sum(pdfD)
  return(pdfD)
}

source(file = "heuristic_defense.R")

sink()
file.show(outputLogFile)
