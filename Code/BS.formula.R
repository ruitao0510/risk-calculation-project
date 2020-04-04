blackScholes<-function(s0,x,sigma,r,t,q){
  d1=(log(s0/x)+t*(r-q+sigma^2/2))/(sigma*sqrt(t))
  d2=d1-sigma*sqrt(t)
  call=s0*exp(-q*t)*pnorm(d1)-x*exp(-r*t)*pnorm(d2)
  put=x*exp(-r*t)*pnorm(-d2)-s0*exp(-q*t)*pnorm(-d1)
  results<-cbind(call,put)
  return(results)
}