#CONFOUNDING!

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
sigma_c2<-sqrt(var_c2)
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

confoundingDiff<-(1+(sigma_g)^2+(sigma_c1)^2)-(1+(sigma_g)^2+(sigma_c2)^2)
confoundingDiffEstimate<-var(data$s1)-var(data$s2)


#calculating s2 given s1
calculate_pcondtional<-function(s1){
  mean<-(sigma_g^2*s1)/(1+sigma_g^2+sigma_c1^2)
  var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  p<- (1-pnorm(5.2, mean, sqrt(var)))+pnorm(-5.2, mean, sqrt(var))
  return(p)
}

#rep rate 
s1_sig<-nrow(filter(data, s1>5.2|s1<(-5.2)))
s2_s1_sig<-nrow(filter(filter(data, s1>5.2), s2>5.2)) + nrow(filter(filter(data, s1<(-5.2)), s2<(-5.2)))
repRate=s2_s1_sig/M

#theoretical reprate
s1_sig_vector <-filter(data, s1>5.2|s1<(-5.2))$s1
theo_rep <- sum(calculate_pcondtional(s1_sig_vector))/M

#visualization
func<-function(s1, c1, c2, g){
  mean<-(g*s1)/(1+g+c1)
  var<-(1-(g^2)/((1+g+c1)*(1+g+c2)))*(1+g+c2)
  p<- (1-pnorm(5.2, mean, sqrt(var)))+pnorm(-5.2, mean, sqrt(var))
  return(p)
}

ggplot(data=data, mapping=aes(x=s1))+stat_function(fun=func, args=list(c1<-0.2, c2<-0, g<-1.8), aes(colour="c1=0.2"))+
  stat_function(fun=func, args=list(c1<-0.0, c2<-.2, g<-1.8), aes(colour="c2=0.2"))+
  stat_function(fun=func, args=list(c1<-0, c2<-0, g<-2), aes(colour="c2=c1=0"))









