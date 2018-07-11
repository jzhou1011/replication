
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


sigma<-sqrt(2)
N=10000
lamda <- rnorm(n=N, mean=0, sd=tempSd)

#to check if the lamda drawn is normally ditrbuted 
den_init_lambda <- density(lamda)
plot(den_init_lambda)

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

true_lambda <- function(lambda){
  return(lambda)
}

#probability of s2 replicating given s1 
#the the two are multivate normal so we can use the equation 
#each s1 has a different distbrution for s2 will be and then we want the probability of that replicating 
calculate_pcondtional<-function(s1){
  mean<-(sigma^2*s1)/(1+sigma^2)
  var=1+((sigma^2)/(1+sigma^2))
  p<-pnorm(5.2, mean, sqrt(var))
  return(p)
}

calculate_pstudy1<-function(s1){
  pnorm(s1,0,sqrt(1+sigma^2))
}


#actual results of simulation 
# number of s1 greater than 5.2 
s1_sig<-nrow(filter(results.data, s1>5.2))
s2_sig_givens1<-nrow(filter(filter(results.data, s1>5.2), s2>5.2))
repRate=s2_sig_givens1/s1_sig

