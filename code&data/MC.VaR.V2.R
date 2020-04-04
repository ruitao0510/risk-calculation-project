##################################### Monte Carlo VaR File ####################################
# This file is designed for calculating Monte Carlo VaR
###############################################################################################

##### Calculate the Monte Carlo VaR and ES on a single date
source("winEst2.R")
source("BS.formula.R")

MC <- function(investment ,reducepct=0,current_date, 
               stock, stockposition, rtn, 
               iv,mat,Type,r,optionshare,
               p.VaR , p.ES , t , l , npaths ,flag) {
  
  # Inverstment: Assume a fixed position in the portfolio, Default = 10000
  # reducepct: The percentage of Inverstment invest in options
  # current_date: The specific date when the Monte Carlo VaR is calculated
  # stock: A dataframe contains the stocks' prices
  # stockposition: A vectory contains the position of stocks
  # logrtn: A dataframe contains the logreturn of stocks
  # iv: Implied volitility of options
  # mat: Time to maturity
  # Type: Put or Call
  # r: risk free rate
  # p.VaR: Confidence Level, Default = 99%
  # p.ES: Confidence level, Default = 97%
  # t: VaR Time Horizon, Default = 5 days (5/252 years)
  # l: Window Length, Default = 5 years
  # npaths: The number of generated random values using Monte Carlo methods, Deafult = 10000
  if(sum(stockposition<0)==length(stockposition)){
    stockpositiontemp <- abs(stockposition) 
  } else {
    stockpositiontemp <- stockposition
  }
  
  logrtn <- log(rtn)
  dt <- t
  if ( flag > 1) {
    # Initialize matrix
    covriance <- matrix(nrow = ncol(rtn), ncol = ncol(rtn))
    simga <- matrix(nrow = ncol(rtn), ncol = ncol(rtn))
    rho <- matrix(nrow = ncol(rtn), ncol = ncol(rtn))
    
    
    # get the volitility and drift
    vol <- unlist(winEst2(rtn, current_date, l,flag)[2]) 
    mu <- unlist(winEst2(rtn, current_date, l,flag)[1]) 
    
    # Caculate stocks' covariance
    endtemp <- min(length(rtn[,1]),l*252+current_date)
    covmatrix <- cov(logrtn[current_date:endtemp,])
    
    # caculate correlated matrix
    volB <-  matrix(rep(vol,length(vol)),nrow = length(vol), byrow = TRUE)
    volA <- matrix(rep(vol,length(vol)), nrow = length(vol))
    volmatrix <- volA*volB
    rho <- 252*(covmatrix/volmatrix)
    diag(rho) <- rep(1,nrow(rho))
    
    # generate random numbers 
    randomsample<-mvrnorm(npaths,rep(0,ncol(stock)),rho)
    
    # caculate the value of portfolio
    volm <- matrix(rep(vol,npaths),ncol = ncol(stock), byrow = TRUE)
    mum <- matrix(rep(mu,npaths),ncol = ncol(stock), byrow = TRUE)
    
    S0 <- sum(stock[current_date,]*stockpositiontemp)
    portfoliotemp <- (exp(randomsample*volm*sqrt(dt)+ (mum - volm^2/2)*dt)) * matrix(rep(stock[current_date,]*stockpositiontemp,npaths),ncol = ncol(stock), byrow = TRUE)
    St <- rowSums(portfoliotemp)
    
  } else {
    vol <- unlist(winEst2(rtn, current_date, l,flag)[2])
    mu <- unlist(winEst2(rtn, current_date, l,flag)[1])
    S0 <- stock[current_date]
    St <- S0*exp(sqrt(dt) * rnorm(npaths,mean=0,sd=1)*vol+ (mu - vol^2/2)*dt)
    
  }

  stockshare <- investment/S0
  Vt.stock <- stockshare*St
  V0.stock <- S0*stockshare
  
  # add options
  
  if (is.na(iv[1])) {
    Vt.option <- 0
    V0.option <- 0
    portfolio <- Vt.stock + Vt.option
    if(sum(stockposition < 0) < length(stockposition)){
      # sort Portfolio (decreasing)
      portsort <- sort(portfolio,decreasing = F)
      
      VaR <- investment+V0.option-portsort[ceiling((1-p.VaR)*npaths)]
      ES <- investment+V0.option-mean(portsort[1:ceiling((1-p.ES)*npaths)])
    } else {
      # sort Portfolio (decreasing)
      portsort <- sort(portfolio,decreasing = F)
      VaR <- -investment-V0.option+portsort[ceiling((p.VaR)*npaths)]
      ES <- -investment-V0.option+mean(portsort[ceiling(p.ES*npaths):npaths])
    }
  } else {
    
    tag <- ifelse(Type=="p",2,1)
    strike <- S0
    put0 <- unlist(blackScholes(S0,strike,iv, r, mat, 0)[,tag])
    putt <- unlist(blackScholes(St,strike,iv, r, mat-dt, 0)[,tag])
    
    Vt.option <- optionshare * putt
    V0.option <- optionshare * put0
    
    portfolio <- Vt.stock + Vt.option
    if(sum(stockposition < 0) < length(stockposition)){
      loss <- V0.option+investment-portfolio
      # sort Portfolio (decreasing)
      portsort <- sort(loss,decreasing = F)
      
      VaR <- quantile(loss, VaRp,na.rm = TRUE)
      ES <- mean(portsort[1:ceiling((1-p.ES)*npaths)])
    } else {
      # sort Portfolio (decreasing)
      loss <- -V0.option-investment+portfolio
      portsort <- sort(loss,decreasing = F)
      
      VaR <- quantile(loss, VaRp,na.rm = TRUE)
      ES <- -investment-V0.option+mean(portsort[ceiling(p.ES*npaths):npaths])
    }
  # get results
  return(c(VaR, ES))
}
