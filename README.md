# ComputersAndSecurity_RoboticsCaseStudies_Cut-The-Rope

This repository contains the code used to run the two case studies in the paper by

Stefan Rass, Sandra Konig, Jasmin Wachter, Víctor Mayoral-Vilches, Emmanouil Panaousis, "Game-Theoretic APT defense: An Experimental Study on Robotics", Computers & Security (2023), doi: https://doi.org/10.1016/j.cose.2023.103328.

The files contained are the following:
- `attack_graph_MIR100.R`   Attack graph from the MIR100 use case
- `attack_graph_MARA.R`     Attack graph from the MARA use case
- `ctr-core_1.R`            Core implementation of the Cut-The-Rope Game (called by the subsequent experimental scripts)
- `HyRiM_2.0.1.tar.gz`      R-Package "HyRiM"; either installable directly from this file, or (better) from the official CRAN repository at https://cran.r-project.org/web/packages/HyRiM/index.html
- `heuristic_defense.R`     Baseline heuristic (uniform defense) from Section 5.1
- `experiment_1.R`          Experiment from Section 5.3, MARA, periodic defender, exponential attacker, results shown in Figure 3
- `experiment_2.R`          Experiment from Section 5.3, MARA, exponential defender, exponential attacker, results shown in Figure 4
- `experiment_3.R`          Experiment from Section 5.4, MIR100, periodic defender, exponential attacker, results shown in Figure 5
- `experiment_4.R`          Experiment from Section 5.4, MIR100, exponential defender, exponential attacker, results shown in Figure 6

To **customize an experiment**, do the following:
1) specify your attack graph as exemplified in the attack_graph_*-files. This will require a graph compatible with the igraph package from CRAN. 
2) specify the adversarial movement pattern as exemplified in the experiment_x.R-files. Additionally, if desired, specify the defender's locations by adding a line `as1 <- c(a,b,c,...)` to restrict the defender's spot checking only to the given locations (here a, b, c, ...).
3) run the game by sourcing your script in R (in general, there is **no need** to change the **core code** of the CTR game)

The files `experiment_x_H.R` are the respective heuristic defenses (Section 5.1) for each experiment
The files endling with `.log` are the outputs as produced by the respective scripts (examples)

To re-run an experiment, just **source the file in R**, but make sure to have **set the working directory** of R to the folder where all the files are located.

In case of questions, please contact stefan.rass@jku.at (corresponding author of the article).
