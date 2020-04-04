##################################### Parametric VaR File ####################################
# This file is designed for calculating parametric VaR
###############################################################################################



##### Calculate the Parametric VaR on a single date
source("winEst.R")
Parametric_VaR<-function(data,current_date,p=0.99,t=5/252,S0=10000){
  
  # data: A dataframe contains the Date, Portfolio_Value, log_return and square_log_return
  # current_date: The specific date when the Parametric VaR is calculated
  # p: Confidence Level, Default = 99%
  # t: VaR Time Horizon, Default = 5 days
  # S0: Assume a fixed position in the portfolio
  
  para<-winEst(data,current_date,l=5)
  
  # Long Portflio
  if(data[current_date,"Portfolio_Value"]>0){
    ans<-S0-S0*exp(para[2]*sqrt(t)*qnorm(1-p)+(para[1]-para[2]^2/2)*t)
  }
  
  # Short portfolio
  else  if(data[current_date,"Portfolio_Value"]<0)
  {
    ans<-S0*exp(para[2]*sqrt(t)*qnorm(p)+(para[1]-para[2]^2/2)*t)-S0
  }
  
  return(ans)
  
}