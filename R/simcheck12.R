###################################
### simcheck12.R: Why are       ###
### Model-based SEs wrong?      ###
###################################

# Load relevant libraries 
library(boot) # Contains inv.logit function
library(mice) # for MI
library(foreach) # for using foreach loop
library(dplyr) # To transform list fo data frame in single data frame
library(rsimsum) # For computing performance measures with MCSE

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

# Next we provide a function to analyse partially observed data.set:

anadata <- function( dataframe, rep) {
  
  # Method 2: CCA  
  fit.cca<-try(glm(D~E+Cobs, family=binomial(link="logit"), data=dataframe, singular.ok=F, epsilon = 1e-14))
  if (!inherits(fit.cca, "try-error")) {
    res<-data.frame(
      rep <- rep,
      method <- "CCA",
      est <- coef(fit.cca)["ETRUE"],
      se <- coef(summary(fit.cca))["ETRUE",2],
      N <- nobs(fit.cca),
      df <- NA, # df is only needed for MI but must be included for all
      row.names = NULL)
  } else {
    res<-data.frame(
      rep <- rep,
      method <- "CCA",
      est <- NA,
      se <- NA,
      N <- NA,
      df <- NA, # df is only needed for MI but must be included for all
      row.names = NULL)
    
  }
  
  # Method 3: MI 
  df.mice<-dataframe[,c("Cobs", "D", "E")]
  df.mice$int<-df.mice$D*df.mice$E
  imp <- try(mice(df.mice, method = "norm", m = 5, printFlag = F))
  if (!inherits(imp, "try-error")) {
    fit <- with(data = imp, exp = glm(D~E+Cobs, family=binomial(link="logit"), singular.ok=F, epsilon = 1e-14))
    if (!inherits(fit, "try-error")) {
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

# Create complete data and find true value 

full.data <- gendata( obs = 500, logite = "-3+Ctrue", logitd = "-1+Ctrue")
fit.fd<-try(glm(D~E+Ctrue, family=binomial(link="logit"), data=full.data, singular.ok=F, epsilon = 1e-14))
if (class(fit.fd)[1]!= "try-error") {
  true<-coef(fit.fd)["ETRUE"]
} else {
  true<-0
}

# Perform simulation (1000 repetitions)

results <- foreach( i = 1:1000) %do% {    # Note that in order to save the seed we return results as a list
  dataframe <- gendata2( dat=full.data, pmiss = 0.3)
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
s <- simsum(data = results.df, estvarname = "logOR", true = true, se = "se", methodvar = "method", ref = "CCA")
summary(s)

