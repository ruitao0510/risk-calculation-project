##################################### Historical VaR File ####################################
# This file is designed for calculating historical VaR
###############################################################################################

# Calculate the Portfolio Return Vector 
returnVector<-function(data,t){
  len<-nrow(data)-t
  vector<-rep(NA,len)
  for(i in 1:len){
    vector[i]<-log(data$Portfolio_Value[i]/data$Portfolio_Value[i+t])
  }
  return(vector)
}


# Calculate the historical VaR on a single date
historical_VaR<-function(data,return.vector,current_date,p=0.99,S0){
  
  # data: A dataframe contains the Date, Portfolio_Value, log_return and square_log_return
  # return.vector: A vector contains the log returns over a given time horizon
  # current_date: The specific date when the Historical VaR is calculated
  # p: Confidence Level, Default = 99%
  # S0: Assume a fixed position in the portfolio
  
  cutoff<-quantile(return.vector,1-p,na.rm=T)
  
  ans<-S0-S0*exp(cutoff)
  
  return(ans)
}




  
  
  