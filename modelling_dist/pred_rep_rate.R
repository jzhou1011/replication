
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


sigma<-sqrt(2)
N=1000000
lamda <- rnorm(n=N, mean=0, sd=sigma)

#to check if the lamda drawn is normally ditrbuted 
#den_init_lambda <- density(lamda)
#plot(den_init_lambda)

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

#probability of s2 replicating given s1 
#the the two are multivate normal so we can use the equation 
#each s1 has a different distbrution for s2 will be and then we want the probability of that replicating 
calculate_pcondtional<-function(s1){
  mean<-(sigma^2*s1)/(1+sigma^2)
  var=1+((sigma^2)/(1+sigma^2))
  p<- (1-pnorm(5.2, mean, sqrt(var)))
  return(p)
}

calculate_pstudy1<-function(s1){
  dnorm(s1,0,sqrt(1+sigma^2))
}

integrand <- function(s1){
  calculate_pcondtional(s1)*calculate_pstudy1(s1)
}

theo_rep <-integrate(integrand, lower=5.2, upper=Inf)
#actual results of simulation 
# number of s1 greater than 5.2
s1_sig<-nrow(filter(results.data, s1>5.2))
s2_sig_givens1<-nrow(filter(filter(results.data, s1>5.2), s2>5.2))
repRate=s2_sig_givens1/s1_sig

s1_sig_vector <-filter(results.data, s1>5.2)$s1
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector))/N


