# Default: Length of Windows" 5 Years
# Estimate the drift and volatility term on each date using 5-year window
# The data is sorted by date from newest to oldest

winEst2<-function(rtn, current_date, l=5,flag){
  
  # data: A dataframe contains the log_return
  # current_date: The specific date when the drift and volatility terms are calculated
  # l: Window Length, Default = 5 years
  # flag: determin the stock is one or more
  
  if (flag > 1 ){
    endtemp <- min(length(rtn[,1]),l*252+current_date)
    logrtn <- log(rtn)
    logrtn2 <- logrtn*logrtn
    # Initial matrix
    mean_log_return <- matrix(NA,nrow = 1, ncol = ncol(logrtn))
    mean_square_log_return <- matrix(NA,nrow = 1, ncol = ncol(logrtn))
    sd_log_return <- matrix(NA,nrow = 1, ncol = ncol(logrtn))
    volatility <- matrix(NA,nrow = 1, ncol = ncol(logrtn))
    drift <- matrix(NA,nrow = 1, ncol = ncol(logrtn))
    
    mean_log_return[1,]<-unlist(apply(logrtn[current_date:endtemp,], 2, mean)) 
    mean_square_log_return[1,]<-unlist(apply(logrtn2[current_date:endtemp,], 2, mean)) 
    sd_log_return[1,]<-sqrt(mean_square_log_return-(mean_log_return)^2)
    
    volatility[1,]<-sd_log_return*sqrt(252)
    drift[1,]<-252*mean_log_return+volatility^2/2
    
  } else {
    endtemp <- min(length(rtn),l*252+current_date)
    
    logrtn <- log(rtn)
    logrtn2 <- logrtn^2
    
    mean_log_return<-mean(logrtn[current_date:endtemp])  
    mean_square_log_return<-mean(logrtn2[current_date:endtemp])
    sd_log_return<-sqrt(mean_square_log_return-(mean_log_return)^2)
    volatility<-sd_log_return*sqrt(252)
    drift<-252*mean_log_return+volatility^2/2
  }
  return(list(drift,volatility))
}

