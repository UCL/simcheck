###################################
### simcheck10.R: Deal with     ###
### Outliers.                   ###
###################################

# Load relevant libraries and source simcheck05.R
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
library(dplyr) # To transform list fo data frame in single data frame
library(logistf) # To use Firth correction
library(miceafter) # To apply Rubin s rules after using logistf
source("simcheck05.R")

# Set seed
set.seed(576819506)

# Changed program to analyse the data using Firth correction:

anadata <- function( dataframe, rep) {
  
  # Method 1: full data before data deletion  
  fit.fd<-try(logistf(D~E+Ctrue, data=dataframe, singular.ok=F))
  if (class(fit.fd)[1]!= "try-error") {
    res<-data.frame(
      rep <- rep,
      method <- "Full",
      est <- coef(fit.fd)["ETRUE"],
      se <- sqrt(diag(summary(fit.fd)$var))[2],
      N <- nobs(fit.fd),
      df <- NA, # df is only needed for MI but must be included for all
      row.names = NULL)
  } else {
    res<-data.frame(
      rep <- rep,
      method <- "Full",
      est <- NA,
      se <- NA,
      N <- NA,
      df <- NA, # df is only needed for MI but must be included for all
      row.names = NULL)
    
  }
  
  # Method 2: CCA 
  fit.cca<-try(logistf(D~E+Cobs, data=dataframe, singular.ok=F))
  if (class(fit.cca)[1]!= "try-error") {
    res<-rbind(res,c(
      rep,
      "CCA",
      coef(fit.cca)["ETRUE"],
      sqrt(diag(summary(fit.cca)$var))[2],
      nobs(fit.cca),
      NA # df is only needed for MI but must be included for all
    ),
    row.names=NULL)
  } else {
    res<-rbind(res,c(
      rep,
      "CCA",
      NA,
      NA,
      NA,
      NA # df is only needed for MI but must be included for all
    ),
    row.names=NULL)
  }
  
  
  # Method 3: MI 
  df.mice<-dataframe[,c("Cobs", "D", "E")]
  df.mice$int<-df.mice$D*df.mice$E
  imp <- try(mice(df.mice, method = "norm", m = 5, printFlag = F))
  if (class(imp)[1] != "try-error") {
    fit <- with(data = imp, exp = logistf(D~E+Cobs, singular.ok=F))
    if (class(fit)[1] != "try-error") {
      coefs<-c(
        fit$analyses[[1]]$coefficients["ETRUE"],
        fit$analyses[[2]]$coefficients["ETRUE"],
        fit$analyses[[3]]$coefficients["ETRUE"],
        fit$analyses[[4]]$coefficients["ETRUE"],
        fit$analyses[[5]]$coefficients["ETRUE"]
      )
      ses<-c(
        sqrt(diag(fit$analyses[[1]]$var))[2],
        sqrt(diag(fit$analyses[[2]]$var))[2],
        sqrt(diag(fit$analyses[[3]]$var))[2],
        sqrt(diag(fit$analyses[[4]]$var))[2],
        sqrt(diag(fit$analyses[[5]]$var))[2]
      )
      rub.rul <- pool_scalar_RR(coefs, ses, dfcom=200-3) # mice pool could not be used with logistf
      res<-rbind(res,c(
        rep,
        "MI",
        rub.rul$pool_est,
        rub.rul$pool_se,
        nobs(fit.fd),
        rub.rul$v_adj), 
        row.names=NULL)
    } else {
      
      res<-rbind(res,c(
        rep,
        "MI",
        NA,
        NA,
        NA,
        NA),
        row.names=NULL)
    }
  } else {
    
    res<-rbind(res,c(
      rep,
      "MI",
      NA,
      NA,
      NA,
      NA),
      row.names=NULL)
  }
  
  colnames(res) <- c( "rep", "method", "est", "se", "N", "df")
  
  return(res)
  
}

# run simulation study
results <- foreach( i = 1:120) %do% {    # Note that in order to save the seed we return results as a list
  dataframe <- gendata( obs = 200, logite = "-4+Ctrue", logitd = "-4+Ctrue", pmiss = 0.3)
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
View(results.df)