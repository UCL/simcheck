###################################
### simcheck06.R: Make it easy  ###
### to re-create any simulated  ###
### data set.                   ###
###################################

# Load relevant libraries and source simcheck05.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
source("simcheck05.R")

# Set seed
set.seed(576819506)

# run 3 repetitions
results <- foreach( i = 1:3) %do% {    # Note that in order to save the seed we return results as a list
  dataframe <- gendata( obs = 500, logite = "-1+Ctrue", logitd = "-1+Ctrue", pmiss = 0.3)
  resul<-anadata(dataframe, i)
  attr(resul, "seed")<-.Random.seed  # Now store the seed after running the sim study, in case you want to continue from here later
  resul
}

# view stored results for 3rd repetition
View(results[[3]])


# reconstruct data set for 3rd repetition and check it gives same results
.Random.seed <- attr(results[[2]], "seed") # Need the seed status after running 2 repetitions
dataframe <- gendata( obs = 500, logite = "-1+Ctrue", logitd = "-1+Ctrue", pmiss = 0.3)
resul<-anadata(dataframe, i)
View(resul)
# can verify that these results are the same
