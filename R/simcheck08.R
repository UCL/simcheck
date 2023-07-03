###################################
### simcheck08.R: Check for     ###
### outliers.                   ###
###################################

# Load relevant libraries, source simcheck05.R and load simcheck08_results.RData
library(boot) # Contains inv.logit function
library(mice) # for MI
library(ggplot2)

source("simcheck05.R")
load("simcheck07_results.RData")

# Find where very large se occurs
large.ses<-unique(results.df[results.df$se>100,"rep"])

# Re-level so that Full is reference:
results.df$method<-factor(results.df$method, levels=c("Full", "CCA", "MI"))

# Scatterplot of SE against estimate
ggplot(results.df,aes(x=est,y=se)) + 
  geom_point(size = 2) + 
  facet_wrap(~method, ncol = 3) + 
  labs(x="Point estimate", y="Standard error estimate")

