#predicting whether s2 replicates given s1. Different Sample Size 

library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 
sigma<-sqrt(.002)
N=50
N_1=10000
zScore=5.2

#probability of s2 replicating given s1
calculate_pcondtional<-function(s1,sampleS1, sampleS2){
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1*sampleS2)*sigma^2*s1)/(sd_S2^2)
  var2<-1+((sampleS1*sigma^2)/(sd_S2)^2)
  p<- (1-pnorm(zScore, mean, sqrt(var2)))+pnorm(-zScore, mean, sqrt(var2))
  return(p)
}


s1_dist<-rnorm(n=N, mean=0, sd=sqrt(sigma^2*N_1+1))
s2_dist1<-rnorm(n=N, mean=0, sd=sqrt(sigma^2*N_1*0.5+1))
s2_dist2<-rnorm(n=N, mean=0, sd=sqrt(sigma^2*N_1*1+1))
s2_dist3<-rnorm(n=N, mean=0, sd=sqrt(sigma^2*N_1*2+1))
s2_dist4<-rnorm(n=N, mean=0, sd=sqrt(sigma^2*N_1*5+1))

table1<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table1)=c("N1", "N2", "S1", "Ps2")
table1$N1<-rep(N_1, N)
table1$N2<-rep(N_1*0.5, N)
table1$S1<-s1_dist
table1$Ps2<-calculate_pcondtional(table1$S1, N_1, table1$N2[1])

P_S1_1<-ggplot(data=table1, mapping=aes(x=S1))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1*0.5))


table2<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table2)=c("N1", "N2", "S1", "Ps2")
table2$N1<-rep(N_1, N)
table2$N2<-rep(N_1, N)
table2$S1<-s1_dist
table2$Ps2<-calculate_pcondtional(table2$S1, N_1, table2$N2[1])

P_S1_2<-ggplot(data=table2, mapping=aes(x=S1))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1))


table3<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table3)=c("N1", "N2", "S1", "Ps2")
table3$N1<-rep(N_1, N)
table3$N2<-rep(N_1*2, N)
table3$S1<-s1_dist
table3$Ps2<-calculate_pcondtional(table3$S1, N_1, table3$N2[1])

P_S1_3<-ggplot(data=table3, mapping=aes(x=S1))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1*2))


table4<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table4)=c("N1", "N2", "S1", "Ps2")
table4$N1<-rep(N_1, N)
table4$N2<-rep(N_1*5, N)
table4$S1<-s1_dist
table4$Ps2<-calculate_pcondtional(table4$S1, N_1, table4$N2[1])





calculate_pstudy1<-function(s1){
  dnorm(s1,0,sqrt(1+sigma^2))
}

integrand <- function(s1){
  calculate_pcondtional(s1)*calculate_pstudy1(s1)
}

#predicting using our formula 
theo_rep <-integrate(integrand, lower=5.2, upper=Inf)$value
theo_rep <- theo_rep + integrate(integrand, lower=(-Inf), upper=(-5.2))$value

#actual results of simulation 
# number of s1 greater than 5.2
s1_sig<-nrow(filter(results.data, s1>5.2|s1<(-5.2)))
s2_sig_givens1<-nrow(filter(filter(results.data, s1>5.2), s2>5.2)) + nrow(filter(filter(results.data, s1<(-5.2)), s2<(-5.2)))
repRate=s2_sig_givens1/s1_sig

s1_sig_vector <-filter(results.data, s1>5.2|s1<(-5.2))$s1
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector))/N


#plotting probabilities 
probabilties_conditional<-ggplot(data=filter(results.data, s1>0 & s1<100), mapping = aes(x=s1))+
  stat_function(fun=calculate_pcondtional)

#plotting means
mean<-function(s1){
  (sigma^2*s1)/(1+sigma^2)
}

means_conditional<-ggplot(data=filter(results.data, s1>5.2 & s1<6), mapping = aes(x=s1))+
  stat_function(fun=mean)

means_conditional

