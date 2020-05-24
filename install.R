# Install dependencies
packages <-
c("hardhat", 
"rlang",
"stats",
"methods",
"Rcpp",
"RcppArmadillo",
"parsnip",
#"tibble",
#"MASS",
"fda.usc",
"clues",
"recipes",
"testthat",
#"dplyr",
#"ggplot2",
"reshape2",
"rsample",
"tune",
#"devtools",
"yardstick"
)

install.packages(packages)
devtools::install_github("jlaria/glasp")

cat("Installation Finished")
