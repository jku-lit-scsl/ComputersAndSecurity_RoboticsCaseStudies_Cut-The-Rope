# experiment 1:
# conducted: 2022-01-24
# setup:
# - adversary: moves at Poisson rate
# - defender: periodic 
# - attack graph: MARA

rm(list = ls())  # clean up the workspace to avoid side-effects

outputLogFile <- "experiment_1.log"

con <- file(outputLogFile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
print(outputLogFile)
print(date())

source(file = "attack_graph_MARA.R")

attackRateList <- 2  # parameter lambda

# random steps determined from attack rate using a Poisson distribution
randomSteps <- function(route, attackRate = NULL, defenseRate = NULL) {
  # the value of "attackRate" comes from an external loop
  pdfD <- dpois(x=0:(length(route)-1), lambda = attackRate)
  pdfD <- pdfD / sum(pdfD)
  return(pdfD)
}

source("ctr-core_1.R")

sink()
file.show(outputLogFile)
