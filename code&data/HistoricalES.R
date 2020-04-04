##################################### Historical ES File ####################################
# This file is designed for calculating historical ES
###############################################################################################


# Calculate the historical ES on a single date
historical_ES<-function(data,return.vector,current_date,p=0.975,S0) 
{
  
  # data: A dataframe contains the Date, Portfolio_Value, log_return and square_log_return
  # return.vector: A vector contains the log returns over a given time horizon
  # current_date: The specific date when the Historical ES is calculated
  # p: Confidence Level, Default = 97.5%
  # S0: Assume a fixed position in the portfolio
  
  v<-quantile(return.vector,1-p,na.rm=T)
  cutoff<-mean(return.vector[return.vector<=v])
  ans<-S0-S0*exp(cutoff)
  
  return(ans)
}