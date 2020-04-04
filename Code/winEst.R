# Default: Length of Windows" 5 Years
# Estimate the drift and volatility term on each date using 5-year window
# The data is sorted by date from newest to oldest

winEst<-function(data, current_date, l=5){
  
  # data: A dataframe contains the Date, Portfolio_Value, log_return and square_log_return
  # current_date: The specific date when the drift and volatility terms are calculated
  # l: Window Length, Default = 5 years
  
  mean_log_return<-mean(data$log_return[current_date:(current_date+l*252-1)])  
  mean_square_log_return<-mean(data$square_log_return[current_date:(current_date+l*252-1)])
  sd_log_return<-sqrt(mean_square_log_return-(mean_log_return)^2)
  volatility<-sd_log_return*sqrt(252)
  drift<-252*mean_log_return+volatility^2/2
  
  return(c(drift,volatility))
}

