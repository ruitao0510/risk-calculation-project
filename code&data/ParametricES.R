##################################### Parametric ES File ####################################
# This file is designed for calculating parametric ES
###############################################################################################


##### Calculate the Parametric ES on a single date
source("winEst.R")
source("ParametricVaR.R")

Parametric_ES<-function(data,current_date,p=0.975,t=5/252,S0=10000){
  
  # data: A dataframe contains the Date, Portfolio_Value, log_return and square_log_return
  # current_date: The specific date when the Parametric ES is calculated
  # p: Confidence Level, Default = 97.5%
  # t: VaR Time Horizon, Default = 5 days
  # S0: Assume a fixed position in the portfolio
  
  para<-winEst(data,current_date,l=5)
  
  # Long Portflio
  if(data[current_date,"Portfolio_Value"]>0){
    X<-S0-Parametric_VaR(data,current_date,p=0.975,t=5/252,S0=10000)
    d1<-(log(S0/X)+(para[1]+para[2]^2/2)*t)/(para[2]*t^0.5)
    ans<-S0-S0*exp(para[1]*t)*(1-pnorm(d1))/(1-p)
  }
  
  # Short portfolio
  else  if(data[current_date,"Portfolio_Value"]<0)
  {
    X<-S0+Parametric_VaR(data,current_date,p=0.975,t=5/252,S0=10000)
    d1<-(log(S0/X)+(para[1]+para[2]^2/2)*t)/(para[2]*t^0.5)
    ans<-S0*exp(para[1]*t)*pnorm(d1)/(1-p)-S0
  }
  
  return(ans)
  
}