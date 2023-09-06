###################################
### simcheck99.R: A complete    ###
### successful simulation study.###
###################################

# Load relevant libraries and source simcheck05.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
library(dplyr) # To transform list of data frame in single data frame
library(rsimsum) # For computing performance measures with MCSE
source("simcheck05.R")

# First, we write a function to generate complete data:

gendata <- function( obs, logite, logitd) {
  
  Ctrue <- rnorm(obs)
  E <- runif(obs) < inv.logit(eval(parse(text=logite)))
  D <- runif(obs) < inv.logit(eval(parse(text=logitd)))
  data.out<-data.frame(D,E,Ctrue)
  
  return(data.out)  # for clarity, but redundant 
  
}

# Second, program to impose the missing data:

gendata2 <- function( dat, pmiss) {
  
  data.out<-dat
  data.out$Cobs<-dat$Ctrue
  data.out$Cobs[runif(nrow(dat))<pmiss]<-NA
  
  return(data.out)  # for clarity, but redundant 
  
}

# Set seed
set.seed(576819506)

# run simulation study
results <- foreach( i = 1:1000) %do% {    # Note that in order to save the seed we return results as a list
  
  full.data <- gendata( obs = 500, logite = "-3+Ctrue", logitd = "-1+Ctrue")
  
  # First, MCAR:
  dataframe <- gendata2( dat = full.data, pmiss=0.3)
  resul1<-anadata(dataframe, i)
  resul1$dgm<-"MCAR"
  
  # Then, MAR:
  dataframe2 <- gendata2( dat = full.data, pmiss=.1+.2*full.data$E+.2*full.data$D)
  resul2<-anadata(dataframe2, i)
  resul2$dgm<-"MAR"

  # Finally MNAR:
  dataframe3 <- gendata2( dat = full.data, pmiss=.2+.2*full.data$Ctrue)
  resul3<-anadata(dataframe3, i)
  resul3$dgm<-"MNAR"
  
  resul<-rbind(resul1,resul2,resul3)
  
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

# correct one outlier to missing
results.df[results.df$se>10,]
results.df$logOR[results.df$se>10] <- NA
results.df$se[results.df$se>10] <- NA

s <- simsum(data = results.df, estvarname = "logOR", true = 0, se = "se", methodvar = "method", ref = "CCA", by="dgm")
summary(s)
