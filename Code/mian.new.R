##################################### Implementation File ####################################
# This file is designed for adopting the risk calculation system. It will call functions in other files.
###############################################################################################

########################################## IMPORTANT #########################################
# The format of data file "equity.csv" and "option.csv" must satisfy the following requirements
# The data must be arranged from newest to oldest
# The data in both files must begin and end with the same date (i.e. they have the same number of rows)
# See other requirements in "Software Design Documentation"
##############################################################################################
# load libraries
library(MASS)
library(ggplot2)

################################ Set the VaR and ES parameters #####################################
# User needs to determine the length of window he/she wants
# For example, VaR and ES for the past 20 years or 10 years or 5 years
# Default Value: 20 years

# User needs to determine the value of input parameters in the calculation of VaR and ES
S0<-10000 # the total amount invested in stocks
h <- 20 #length of window
p.VaR<-0.99
p.ES<-0.975
t<-5/252 # lookback days(in year)
l<-5 
npaths <- 10000 # generate how many data per date to calculate Monte Carlo VaR and ES
bktesting.length <- 1 # Back testing length deacult as 1

################################### Set the working directory #########################################
# Set the working directory in the following way: "Session->Set Working Directory->Choose Directory"

# Input the data
# Equity data, type the name of the equity file
data.stock<-read.csv("test5_stock.csv",as.is=T)
# Option data, type the name of the option file
data.option<-read.csv("test5_option.csv",as.is=T)

# The user needs to choose whether to input parameters manually or use historical data
# to calibrate the parameters
Input<-"F" # change to T if user want to imput parameters manually


# portfolio_mu: drift term for the GBM model for the entire portfolio. It is a number
# portfolio_sigma: volatility term for the GBM model for the entire portfolio. It is a number
# individual_mu: a array of drift terms for each individal stock in the portfolio
# individual_sigma: a array of volatility terms for each individual stock in the portfolio
# individual_rho: correlation matrix for each individual stock. It is a N*N matrix, where
# N is the number of individual stocks. All the diagonal cell is one

if(Input=="T"){
  # if Input variable is True, then type the mu, sigma and correlation matrix 
  # for portfolio and individual stock
  
  portfolio_mu<-0.05
  portfolio_sigma<-0.2
  individual_mu<-c(0.05,0.03)
  individual_sigma<-c(0.15,0.25)
  individual_rho<-matrix(c(1,0.8,0.8,1),nrow=2) 
}

################################### Calculate VaR and Back test #########################################
####################################### Input Data ##################################################

# Calculate the Portfolio Value
source("DataPrepare.R")
data.stock<-StockValue(data.stock)
data.option<-OptionValue(data.stock,data.option)
data.portfolio<-Portfolio_Value(data.stock, data.option)

# Get stocks' price and position and logreturn
if (ncol(data.stock) > 3 ){
  stock <- matrix(unlist(stockm(data.stock)[1]), ncol = (ncol(data.stock)-2)/2, byrow = FALSE)
  stockposition <- unlist(stockm(data.stock)[2])
  stockrtn <- matrix(unlist(stockm(data.stock)[4]), ncol = (ncol(data.stock)-2)/2, byrow = FALSE)
} else {
  lengthtemp <- nrow(data.stock)
  stockposition <- data.stock[2,3]
  stock <- data.stock[,2]
  stock1d <- data.stock[-1,2]
  stockrtn <- data.stock[-lengthtemp,2]/stock1d
  stocklogrtn <- log(stockrtn)
}

####################################### Parametric VaR and ES #######################################

source("winEst.R")
source("ParametricVaR.R")
source("ParametricES.R")

##########################################################################################################
VaR_length<-min(20*252, nrow(data.portfolio)-l*252)
# Rearrage the date from oldest to newest
date_series<-data.portfolio$Date[1:VaR_length] 
date_series<-rev(date_series)
date_series<-as.Date(date_series)

# Calculate the parametric VaR and ES for the past 20 years
ParaVaR<-rep(NA,VaR_length)
ParaES<-rep(NA,VaR_length)

for(i in 1:VaR_length){
  ParaVaR[i]<-Parametric_VaR(data.portfolio,i,p.VaR,t,S0)
}

for(i in 1:VaR_length){
  ParaES[i]<-Parametric_ES(data.portfolio,i,p.ES,t,S0)
}

# Rearrange the Parametric VaR and ES from oldest to newest
ParaVaR<-rev(ParaVaR)
ParaES<-rev(ParaES)

# Build the Parametric VaR data.frame
ParaVaR.df<-data.frame(date_series,ParaVaR)
colnames(ParaVaR.df)<-c("Date","Parametric_VaR")

# Build the Parametric ES data.frame
ParaES.df<-data.frame(date_series,ParaES)
colnames(ParaES.df)<-c("Date","Parametric_ES")

####################################### Historical VaR and ES ############################################
source("HistoricalVaR.R")
source("HistoricalES.R")
##########################################################################################################
hist.t<-t*252 
num.return<-min(252*l,nrow(data.portfolio))

# Calculate the Portfolio Return Vector
portfolio.return<-returnVector(data.portfolio,hist.t)

# Calculate the Historical VaR and ES for the past 20 years,i.e.VaR_length=252*20=5040

HistVaR<-rep(NA,VaR_length)
HistES<-rep(NA,VaR_length)

for(i in 1:VaR_length){
  HistVaR[i]<-historical_VaR(data.portfolio,portfolio.return[i:(i+num.return-1)],i,p.VaR,S0)
}

for(i in 1:VaR_length){
  HistES[i]<-historical_ES(data.portfolio,portfolio.return[i:(i+num.return-1)],i,p.ES,S0) 
}

# Rearrange the Historical VaR and ES from oldest to newest
HistVaR<-rev(HistVaR)
HistES<-rev(HistES)

# Build the Historical VaR data.frame
HistVaR.df<-data.frame(date_series,HistVaR)
colnames(HistVaR.df)<-c("Date","Historical_VaR")

# Build the Historical ES data.frame
HistES.df<-data.frame(date_series,HistES)
colnames(HistES.df)<-c("Date","Historical_ES")


####################################### Monte Carlo VaR and ES ############################################
source("MC.VaR.R")
##########################################################################################################

# Initialize matrix of final results
MC.results <- matrix(NA,ncol = 2, nrow = VaR_length)

# Calculate the Monte VaR and ES
Type <- ifelse(data.option[2,4]>0,"p","c")
flag =( ncol(data.stock)-2)/2
for (i in 1:VaR_length) {
  MC.results[i,] <- MC(investment = 10000,reducepct =0,current_date=i, 
                       stock=stock, stockposition=stockposition, rtn=stockrtn, 
                       iv=data.option[i,3]/100,mat=1,Type,r=0.005,optionshare=data.option[1,3],
                       p.VaR = 0.99, p.ES = 0.97, t = 5/252, l = 5, npaths = 10000,flag)
}


# Get VaR and ES 
MC.VaR <- rev(MC.results[,1])
MC.ES <- rev(MC.results[,2])

####################################### Back Testing #######################################################
# Caculate real loss 
length <- length(data.portfolio[,1])
realloss <- (1-data.portfolio[1:(length-hist.t),2]/data.portfolio[(hist.t+1):length,2])*10000  

# Caculated how many times that real loss exceed VaR in h year
count.ParaVaR<-c()
count.HistVaR<-c()
count.MC.VaR<-c()
count.ParaES<-c()
count.HistES<-c()
count.MC.ES<-c()

for (i in 1:VaR_length) {
  count.ParaVaR[i] <- sum(realloss[i:(i+bktesting.length*252-1)]>rev(ParaVaR)[(i+5):(i+bktesting.length*252+4)])
  count.HistVaR[i] <- sum(realloss[i:(i+bktesting.length*252-1)]>rev(HistVaR)[(i+5):(i+bktesting.length*252+4)]) 
  count.MC.VaR[i] <- sum(realloss[i:(i+bktesting.length*252-1)]>rev(ParaES)[(i+5):(i+bktesting.length*252+4)]) 
  count.ParaES[i] <- sum(realloss[i:(i+bktesting.length*252-1)]>rev(HistES)[(i+5):(i+bktesting.length*252+4)]) 
  count.HistES[i] <- sum(realloss[i:(i+bktesting.length*252-1)]>rev(MC.VaR)[(i+5):(i+bktesting.length*252+4)]) 
  count.MC.ES[i] <- sum(realloss[i:(i+bktesting.length*252-1)]>rev(MC.ES)[(i+5):(i+bktesting.length*252+4)]) 
}



########################################################################################################
realloss<-realloss[VaR_length:1]
temp.df<-data.frame(date_series,ParaVaR,HistVaR,ParaES,HistES,MC.VaR,MC.ES,realloss)
colnames(temp.df)[1]<-c("Date")
temp.df2<-data.frame(date_series,rev(count.ParaVaR),rev(count.HistVaR),rev(count.MC.VaR),rev(count.ParaES),rev(count.HistES),rev(count.MC.ES))
colnames(temp.df2)[1]<-c("Date")


################################### Graph the results ##############################################

# Comparison between Portfolio Parametric VaR and ES
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=MC.ES,color="Monte Carlo ES"))+
  geom_line(mapping=aes(x=Date,y=ParaES,color="Parametric ES"))+
  geom_line(mapping=aes(x=Date,y=HistES,color="Historical ES"))+
  labs(title="ES",x="Date",y="ES")

ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=MC.VaR,color="Monte Carlo VaR"))+
  geom_line(mapping=aes(x=Date,y=ParaVaR,color="Parametric VaR"))+
  geom_line(mapping=aes(x=Date,y=HistVaR,color="Historical VaR"))+
  labs(title="VaR",x="Date",y="VaR")

# Actually Loss VS caculated VaR 
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=ParaVaR,color="Parametric VaR"))+
  geom_line(mapping=aes(x=Date,y=realloss,color="Actual Loss"))+
  labs(title="Parametric VaR Vs Acutal Loss",x="Date",y="Loss")
# Monte Carlo VaR Vs Acutal Loss
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=MC.VaR,color="Monte Carlo VaR"))+
  geom_line(mapping=aes(x=Date,y=realloss,color="Actual Loss"))+
  labs(title="Monte Carlo VaR Vs Acutal Loss",x="Date",y="Loss")
# Historical VaR Vs Acutal Loss
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=HistVaR,color="Historical VaR"))+
  geom_line(mapping=aes(x=Date,y=realloss,color="Actual Loss"))+
  labs(title="Historical VaR Vs Acutal Loss",x="Date",y="Loss")


# Exceptions Per Year: Parametric VaR
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=rev(count.ParaVaR),color="Parametric VaR"))+
  geom_line(mapping=aes(x=Date,y=((1-p.VaR)*bktesting.length*252),color="Expection"))+
  labs(title="Exceptions Per Year: Parametric VaR",x="Date",y="")
# Exceptions Per Year: Historical VaR
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=rev(count.HistVaR),color="Historical VaR"))+
  geom_line(mapping=aes(x=Date,y=((1-p.VaR)*bktesting.length*252),color="Expection"))+
  labs(title="Exceptions Per Year: Historical VaR",x="Date",y="")
# Exceptions Per Year: Monte Carlo VaR
ggplot(temp.df)+
  geom_line(mapping=aes(x=Date,y=rev(count.MC.VaR),color="Monte Carlo VaR"))+
  geom_line(mapping=aes(x=Date,y=((1-p.VaR)*bktesting.length*252),color="Expection"))+
  labs(title="Exceptions Per Year: Monte Carlo VaR",x="Date",y="")


