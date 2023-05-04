###################################
### simcheck11.R: Check Monte   ###
### Carlo errors.               ###
###################################

# Load relevant libraries and source simcheck05.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
library(dplyr) # To transform list fo data frame in single data frame
library(rsimsum) # For computing performance measures with MCSE
source("simcheck05.R")

# Set seed
set.seed(576819506)

# run simulation study
results <- foreach( i = 1:100) %do% {    # Note that in order to save the seed we return results as a list
  dataframe <- gendata( obs = 500, logite = "-3+Ctrue", logitd = "-1+Ctrue", pmiss = 0.3)
  resul<-anadata(dataframe, i)
  attr(resul, "seed")<-.Random.seed  # Now store the seed after running the sim study, in case you want to continue from here later
  cat(".")
  if (i%%50==0) cat("\n")
  resul
}
results.df<-bind_rows(results, .id = "rep")
results.df[,3]<-as.numeric(results.df[,3])
results.df[,4]<-as.numeric(results.df[,4])
results.df[,5]<-as.numeric(results.df[,5])
results.df[,6]<-as.numeric(results.df[,6])
summary(results.df)

colnames(results.df)[which(colnames(results.df)=="est")]<-"logOR"
s <- simsum(data = results.df, estvarname = "logOR", true = 0, se = "se", methodvar = "method", ref = "Full")
summary(s)