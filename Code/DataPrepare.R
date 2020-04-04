##################################### Data Preparation File ####################################
# This file is designed for preparing the data.portfolio dataframe.
###############################################################################################

######## Get the stocks' prices ######
stockm <- function (data) {
  # Identify how may stocks we input and the length of the data
  stocksnum <- (ncol(data)-2)/2
  n <- nrow(data)

    # Seperate Stocks prices and their positions
    stock <- matrix(nrow = n, ncol = stocksnum)
    stockposition <- matrix(nrow = 1, ncol = stocksnum)
    for (i in 1:stocksnum){
      temp <- 2*i
      stock[,i] <- data.stock[,temp]
      temp <- temp+1
      stockposition[1,i] <- data.stock[1,temp]
    }
    
    # Caculate stocks' return seperately
    stock1d <- stock[-1,]
    stockrtn <- stock[-n,]/stock1d
    stocklogrtn <- log(stockrtn)
  
  return(list(stock,stockposition,stocklogrtn,stockrtn))
}



######## Option Price Calculation ######

BSModel<-function(Type,stockPrice,K,r,vol,T){
  d1<-(log(stockPrice/K)+(r+vol^2/2)*T)/(vol*T^0.5)
  d2<-d1-vol*T^0.5
  if(Type=="c"){
    ans<-pnorm(d1)*stockPrice-pnorm(d2)*K*exp(-r*T)
  }
  else if(Type=="p"){
    ans<-pnorm(-d2)*K*exp(-r*T)-pnorm(-d1)*stockPrice
  }
  return(ans)
}

###### Calculate the total value of stock position
StockValue<-function(StockData){
  
  num.stock<-(ncol(StockData)-1)/2
  
  for(i in 1:nrow(StockData)){
  stock_value<-rep(NA,num.stock)
  for(index in 1:num.stock){
    stock_value[index]<-StockData[i,2*index]*StockData[i,(2*index+1)]
  }
  
  StockData$Stock_Value[i]<-sum(stock_value)
  }
  
  colnames(StockData)[1]<-c("Date")
  
  return(StockData)
}

####### Calculate the total value of option position
OptionValue<-function(StockData,OptionData){
  
  num.option<-(ncol(OptionData)-2)/7
  
  for(i in 1:nrow(OptionData)){
  # Calculate the option price
    option_value<-rep(NA, num.option)
  for(index in 1:num.option){
    # Call Option Value
    call_part<-BSModel("c",StockData[i,2*index],OptionData[i,(7*index-2)],
                       OptionData[i,2],OptionData[i,(7*index-4)],
                       OptionData[i,(7*index-1)])*OptionData[i,(7*index-3)]
    # Put Option Value
    put_part<-BSModel("p",StockData[i,2*index],OptionData[i,(7*index+1)],
                      OptionData[i,2],OptionData[i,(7*index-4)],
                      OptionData[i,(7*index+2)])*OptionData[i,7*index]
    
    # Set NA equal to 0
    call_part<-ifelse(is.na(call_part),0,call_part)
    put_part<-ifelse(is.na(put_part),0,put_part)
    option_value[index]<-call_part+put_part
  }
    OptionData$Option_Value[i]<-sum(option_value)
  }
  
  
  OptionData$Option_Value<-ifelse(is.na(OptionData$Option_Value)==T,0,OptionData$Option_Value)
  
  colnames(StockData)[1]<-c("Date")
  
  return(OptionData)
}

##### Calculate the Portfolio (Stock + Option) Value and Create a Portfolio Dataframe
Portfolio_Value<-function(StockData, OptionData){
  
  portfolio.value<-StockData$Stock_Value+OptionData$Option_Value
  portfolio.date<-StockData$Date
  df<-data.frame(portfolio.date,portfolio.value)
  colnames(df)<-c("Date","Portfolio_Value")
  
  # Calculate the Log Return and Squared Log Return of the Portfolio
  for(i in 1:(nrow(df)-1)){
    df$log_return[i]<-log(df$Portfolio_Value[i]/df$Portfolio_Value[i+1])
    df$square_log_return[i]<-df$log_return[i]^2
  }
  
  return(df)
}


