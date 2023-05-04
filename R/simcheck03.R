###################################
### simcheck03.R: Simulate      ###
### and analyse a single data   ###
### set.                        ###
###################################

# Load relevant libraries and source simcheck02.R
library(boot) # Contains inv.logit function
library(mice) # for MI
source("simcheck02.R")

# Set seed
set.seed(576819506)


# generate a single large data set
dataframe <- gendata( obs = 100000, logite = "-1+Ctrue", logitd = "-1+Ctrue", pmiss = 0.3)


# summarise the data
summary(dataframe)
addmargins(table(dataframe[is.na(dataframe$Cobs),"D"],dataframe[is.na(dataframe$Cobs),"E"])) 
addmargins(table(dataframe[!is.na(dataframe$Cobs),"D"],dataframe[!is.na(dataframe$Cobs),"E"])) 
fit.unad <- glm(D~E, data=dataframe, family=binomial(link="logit"))
summary(fit.unad)
confint(fit.unad)

# fit the data generating models
glm(E~Ctrue, data=dataframe, family=binomial(link="logit"))
glm(D~Ctrue, data=dataframe, family=binomial(link="logit"))


# analyse the data
results <- anadata(dataframe, 1)
results