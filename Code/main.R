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
data.stock<-read.csv("test1_stock.csv",as.is=T)
# Option data, type the name of the option file
data.option<-read.csv("test1_option.csv",as.is=T)

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
source("calculation.R")
temp.df<-calculation(S0,h,p.VaR,p.ES,t,l,npaths,bktesting.length,data.stock,data.option,type="VaR") 
temp.df2<-calculation(S0,h,p.VaR,p.ES,t,l,npaths,bktesting.length,data.stock,data.option,type="Back test") 



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


