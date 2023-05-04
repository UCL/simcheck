###################################
### simcheck02.R: script for    ###
### simulating and analysing    ###
### data.                       ###
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

# Next we provide a function to analyse partially observed data.set:

anadata <- function( dataframe, rep) {
  
  # Method 1: full data before data deletion  
  fit.fd<-glm(D~E+Ctrue, family=binomial(link="logit"), data=dataframe, singular.ok=F)
  res<-data.frame(
    rep <- rep,
    method <- "Full",
    est <- coef(fit.fd)["ETRUE"],
    se <- coef(summary(fit.fd))["ETRUE",2],
    N <- nobs(fit.fd),
    df <- NA, # df is only needed for MI but must be included for all
  row.names = NULL)
  
  # Method 2: CCA 
  fit.cca<-glm(D~E+Cobs, family=binomial(link="logit"), data=dataframe, singular.ok=F)
  res<-rbind(res,c(
   rep,
    "CCA",
    coef(fit.cca)["ETRUE"],
    coef(summary(fit.cca))["ETRUE",2],
    nobs(fit.cca),
    NA # df is only needed for MI but must be included for all
    ),
  row.names=NULL)
  
  # Method 3: MI 
  df.mice<-dataframe[,c("Cobs", "D", "E")]
  df.mice$int<-df.mice$D*df.mice$E
  imp <- mice(df.mice, method = "norm", m = 5, printFlag = F)
  fit <- with(data = imp, exp = glm(D~E+Cobs, family=binomial(link="logit"), singular.ok=F))
  rub.rul<-summary(pool(fit))
  res<-rbind(res,c(
    rep,
    "MI",
    rub.rul[rub.rul$term=="ETRUE","estimate"],
    rub.rul[rub.rul$term=="ETRUE","std.error"],
    nobs(fit.fd),
    rub.rul[rub.rul$term=="ETRUE","df"]),
  row.names=NULL)
  
  colnames(res) <- c( "rep", "method", "est", "se", "N", "df")
  return(res)
  
}