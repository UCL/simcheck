###################################
### simcheck07.R: Assess method ###
### failures.                   ###
###################################
  
# Load relevant libraries and source simcheck05.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
library(dplyr) # To transform list for data frame in single data frame
source("simcheck05.R")

# Set seed
set.seed(576819506)

# run simulation study
# we increase reps to 120 for illustrative purposes
results <- foreach( i = 1:120) %do% {    # Note that in order to save the seed we return results as a list
  dataframe <- gendata( obs = 200, logite = "-4+Ctrue", logitd = "-4+Ctrue", pmiss = 0.3)
  resul<-anadata(dataframe, i)
  attr(resul, "seed")<-.Random.seed  # Now store the seed after running the sim study, in case you want to continue from here later
  cat(".")  # To create progress bar. This can also be done with utils::txtProgressBar()` 
  if (i%%50==0) cat("\n")
  resul
}
results.df<-bind_rows(results, .id = "rep")
results.df[,3]<-as.numeric(results.df[,3])
results.df[,4]<-as.numeric(results.df[,4])
results.df[,5]<-as.numeric(results.df[,5])
results.df[,6]<-as.numeric(results.df[,6])
summary(results.df)

# Inspect results for method failures
View(results.df[is.na(results.df$est),])
View(results.df[results.df$rep==110,])


# Reconstruct one data set with method failures
.Random.seed <- attr(results[[109]], "seed") # Need the seed status after running 109 repetitions
dataframe <- gendata( obs = 200, logite = "-4+Ctrue", logitd = "-4+Ctrue", pmiss = 0.3)

# and explore it
addmargins(table(dataframe[!is.na(dataframe$Cobs),"D"],dataframe[!is.na(dataframe$Cobs),"E"])) 
fit.ad <- glm(D~E+Cobs, data=dataframe, family=binomial(link="logit"))
summary(fit.ad)

# Save results
save(results, results.df, file="simcheck07_results.RData")

# Export results for Stata
library(foreign) # read/write data to different formats
write.dta(results.df, "simcheck07_Rresults.dta") # to Stata format
