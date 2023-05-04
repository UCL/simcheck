###################################
### simcheck05.R: script for    ###
### anticipating analysis       ###
### failures.                   ###
###################################

# Load relevant libraries
library(boot) # Contains inv.logit function
library(mice) # for MI

# First, we write a function to generate a single data set:

gendata <- function( obs, logite, logitd, pmiss ) {
  
  Ctrue <- rnorm(obs)
  E <- runif(obs) < inv.logit(eval(parse(text=logite)))
  D <- runif(obs) < inv.logit(eval(parse(text=logitd)))
  Cobs<-Ctrue
  Cobs[runif(obs)<pmiss]<-NA
  data.out<-data.frame(Cobs,D,E,Ctrue)
  
  return(data.out)  # for clarity, but redundant 
  
}

# Next we provide a function to analyse partially observed data.set anticipating possible failures:

anadata <- function( dataframe, rep) {
  
  # Method 1: full data before data deletion  
  fit.fd<-try(glm(D~E+Ctrue, family=binomial(link="logit"), data=dataframe, singular.ok=F))
  # singular.ok=F is recommended in simulations: it stops R from dropping a variable for multicollinearity
  if (class(fit.fd)[1]!= "try-error") {
    res<-data.frame(
      rep <- rep,
      method <- "Full",
      est <- coef(fit.fd)["ETRUE"],
      se <- coef(summary(fit.fd))["ETRUE",2],
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
  fit.cca<-try(glm(D~E+Cobs, family=binomial(link="logit"), data=dataframe, singular.ok=F))
  if (class(fit.cca)[1]!= "try-error") {
    res<-rbind(res,c(
      rep,
      "CCA",
      coef(fit.cca)["ETRUE"],
      coef(summary(fit.cca))["ETRUE",2],
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
    fit <- with(data = imp, exp = glm(D~E+Cobs, family=binomial(link="logit"), singular.ok=F))
    if (class(fit)[1] != "try-error") {
      rub.rul<-summary(pool(fit))
      res<-rbind(res,c(
        rep,
        "MI",
        rub.rul[rub.rul$term=="ETRUE","estimate"],
        rub.rul[rub.rul$term=="ETRUE","std.error"],
        nobs(fit.fd),
        rub.rul[rub.rul$term=="ETRUE","df"]),
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