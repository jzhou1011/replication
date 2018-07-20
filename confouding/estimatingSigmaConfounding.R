#libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


#simulating data 
var_g<-1.8
var_c1<-0.2
var_c2<-0.1
sigma_g<-sqrt(var_g)
sigma_c1<-sqrt(var_c1) #first study
sigma_c2<-sqrt(var_c2) #second study 
M<-1000000 #number of snps

#generating lambda
lambda <- rnorm(n=M, mean=0, sd=sigma_g)
#generating C1
delta1 <- rnorm(n=M, mean=0, sd=sigma_c1)
#simulating C2
delta2 <- rnorm(n=M, mean=0, sd=sigma_c2)

#generating two test statistics with different confounders
data<-matrix(nrow=M, ncol=5)

for(i in 1:M){
  data[i,1]<-lambda[i]
  data[i,2]<-delta1[i]
  data[i,3]<-delta2[i]
  data[i,4]<-rnorm(n=1, mean=lambda[i]+delta1[i], sd=1) #s1
  data[i,5]<-rnorm(n=1, mean=lambda[i]+delta2[i], sd=1) #s2
}

data=as.data.frame(data)
colnames(data)=c("lambda", "delta1", "delta2", "s1", "s2")
data$avg<-(data$s1+data$s2)*0.5


#calculating s2 given s1
calculate_pcondtional<-function(s1){
  mean<-(sigma_g^2*s1)/(1+sigma_g^2+sigma_c1^2)
  var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  p<- (1-pnorm(5.2, mean, sqrt(var)))+pnorm(-5.2, mean, sqrt(var))
  return(p)
}


#estimating sigma g from having two confounders 
#variance of (s1+s2)-(s1-s2)
sigma_g_est=(var(data$s1+data$s2)-var(data$s1-data$s2))*(1/4)
var_c1_est=var(data$s1)-1-sigma_g_est
var_c2_est=var(data$s2)-1-sigma_g_est

#estimate sig 5 using just the average of s1 and s2 
sigma_g_est2<-var(data$avg)-0.5


rnorm(var,sqrt(2*var))
criticalValue=1.959964
chiSquaredUpper<-((criticalValue+sqrt(2*(M-1)-1))^2)/2
chiSquaredLower<-((-criticalValue+sqrt(2*(M-1)-1))^2)/2



compute_confidence<-function(var){
  upperBound<-((M-1)*var)/chiSquaredLower
  lowerBound<-((M-1)*var)/chiSquaredUpper
  confidence<-c(lowerBound,upperBound, upperBound-lowerBound)
  return(confidence)
}

compute_confidence(var(data$avg))
compute_confidence(sigma_g_est)







