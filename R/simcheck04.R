###################################
### simcheck04.R: Run a few     ###
### iterations.                 ###
###################################

# Load relevant libraries and source simcheck02.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
source("simcheck02.R")

# Set seed
set.seed(576819506)

# run 3 repetitions
results <- foreach( i = 1:3, .combine="rbind") %do% {
  dataframe <- gendata( obs = 500, logite = "-1+Ctrue", logitd = "-1+Ctrue", pmiss = 0.3)
  anadata(dataframe, i)

}

# view results
View(results)


