# experiment 2_H:
# conducted: 2022-01-24
# setup:
# - adversary: takes the shortest/easiest route
# - defender: spotchecks locations uniformly at random and in exponentially distributed time intervals
# - attack graph: MARA

rm(list = ls())  # clean up the workspace to avoid side-effects

outputLogFile <- "experiment_2_H.log"

con <- file(outputLogFile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
print(outputLogFile)
print(date())

source(file = "attack_graph_MARA.R")

attackRateList <- 2  # parameter lambda
# let the defender move slower (lamdba=1), equally fast (lambda=2) or faster (lambda=3)
defenseRateList <- c(1,2,3) # parameter lambda_D

# random steps determined by a geometric distribution (randomly moving defender)
randomSteps <- function(route, attackRate = NULL, defenseRate = NULL) {
  pdfD <- dgeom(x = 0:(length(route) - 1), prob = defenseRate / (attackRate + defenseRate))
  pdfD <- pdfD / sum(pdfD)
  return(pdfD)
}

source("heuristic_defense.R")

sink()
file.show(outputLogFile)
