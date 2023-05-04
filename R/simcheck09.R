###################################
### simcheck09.R: Investigate   ###
### outliers.                   ###
###################################

# Load relevant libraries and source simcheck05.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
library(dplyr) # To transform list fo data frame in single data frame

load("simcheck07_results.RData")

# Find where very large se occurs
large.ses<-unique(results.df[results.df$se>100,"rep"])

# let's pick out the second rep
results.df[results.df$rep==2,]

# Reconstruct this data set with method failures
.Random.seed <- attr(results[[1]], "seed") # Need the seed status after running 1 repetitions
dataframe <- gendata( obs = 200, logite = "-4+Ctrue", logitd = "-4+Ctrue", pmiss = 0.3)

# and explore it
addmargins(table(dataframe[!is.na(dataframe$Cobs),"D"],dataframe[!is.na(dataframe$Cobs),"E"])) 
fit.ad <- glm(D~E+Cobs, data=dataframe, family=binomial(link="logit"), singular.ok=F)
summary(fit.ad)


