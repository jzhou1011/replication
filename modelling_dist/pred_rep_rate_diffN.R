#predicting whether s2 replicates given s1. Different Sample Size 

library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 
sigma<-sqrt(2)
N=1000000
N_1=1
#N_1=10000
z_score=5.2

#probability of s2 replicating given s1
# calculate_pcondtional<-function(s1,sampleS1, sampleS2){
#   sd_S1<-sqrt(sampleS1*sigma^2+1)
#   sd_S2<-sqrt(sampleS2*sigma^2+1)
#   mean<-(sqrt(sampleS1*sampleS2)*sigma^2*s1)/(sd_S1^2)
#   var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
#   p<- (1-pnorm(zScore, mean, sqrt(var2)))+pnorm(-zScore, mean, sqrt(var2))
#   return(p)
# }

calculate_pcondtional<-function(s1,sampleS1, sampleS2){
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
      p[i]<-(1-pnorm(z_score, mean[i], sqrt(var2)))
    }
    else{
      p[i]<-pnorm(-z_score, mean[i], sqrt(var2))
    }
  }
  return(p)
}

lamda <- rnorm(n=N, mean=0, sd=sigma)

s1_dist<-matrix(nrow=N, ncol=1)
s2_dist1<-matrix(nrow=N, ncol=1)
s2_dist2<-matrix(nrow=N, ncol=1)
s2_dist3<-matrix(nrow=N, ncol=1)
s2_dist4<-matrix(nrow=N, ncol=1)

for (i in 1:N){
  s1_dist[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_1),sd=1)
  s2_dist1[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_1*0.5),sd=1)
  s2_dist2[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_1),sd=1)
  s2_dist3[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_1*2),sd=1)
  s2_dist4[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_1*5),sd=1)
}

table1<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table1)=c("N1", "N2", "S1", "Ps2")
table1$N1<-rep(N_1, N)
table1$N2<-rep(N_1*0.5, N)
table1$S1<-as.vector(s1_dist)
table1$Ps2<-calculate_pcondtional(table1$S1, N_1, table1$N2[1])

#actual replication rate table1
table1$S2<-as.vector(s2_dist1)
s1_sig<-nrow(filter(table1, S1>5.2|S1<(-5.2)))
s2_s1_sig<-nrow(filter(filter(table1, S1>5.2), S2>5.2)) + nrow(filter(filter(table1, S1<(-5.2)), S2<(-5.2)))
repRate=s2_s1_sig/N

#theoretical 
s1_sig_vector <-filter(table1, S1>5.2|S1<(-5.2))$S1
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector,sampleS1 = N_1, sampleS2 = N_1*0.5))/N


table2<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table2)=c("N1", "N2", "S1", "Ps2")
table2$N1<-rep(N_1, N)
table2$N2<-rep(N_1, N)
table2$S1<-as.vector(s1_dist)
table2$Ps2<-calculate_pcondtional(table2$S1, N_1, table2$N2[1])

#actual replication rate table2
table2$S2<-as.vector(s2_dist2)
s1_sig<-nrow(filter(table2, S1>5.2|S1<(-5.2)))
s2_s1_sig<-nrow(filter(filter(table2, S1>5.2), S2>5.2)) + nrow(filter(filter(table2, S1<(-5.2)), S2<(-5.2)))
repRate=s2_s1_sig/N

#theoretical 
s1_sig_vector <-filter(table2, S1>5.2|S1<(-5.2))$S1
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector,sampleS1 = N_1, sampleS2 = N_1))/N

table3<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table3)=c("N1", "N2", "S1", "Ps2")
table3$N1<-rep(N_1, N)
table3$N2<-rep(N_1*2, N)
table3$S1<-as.vector(s1_dist)
table3$Ps2<-calculate_pcondtional(table3$S1, N_1, table3$N2[1])

#actual replication rate table3
table3$S2<-as.vector(s2_dist3)
s1_sig<-nrow(filter(table3, S1>5.2|S1<(-5.2)))
s2_s1_sig<-nrow(filter(filter(table3, S1>5.2), S2>5.2)) + nrow(filter(filter(table3, S1<(-5.2)), S2<(-5.2)))
repRate=s2_s1_sig/N

#theoretical 
s1_sig_vector <-filter(table3, S1>5.2|S1<(-5.2))$S1
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector,sampleS1 = N_1, sampleS2 = N_1*2))/N

table4<-as.data.frame(matrix(nrow=N, ncol=4))
colnames(table4)=c("N1", "N2", "S1", "Ps2")
table4$N1<-rep(N_1, N)
table4$N2<-rep(N_1*5, N)
table4$S1<-s1_dist
table4$Ps2<-calculate_pcondtional(table4$S1, N_1, table4$N2[1])


#plotting probabilities 
probabilties_conditional<-ggplot(data=filter(table2, S1>0 & S1<100), mapping = aes(x=S1))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1))

probabilties_conditional<-ggplot(data=filter(table3, S1>0 & S1<100), mapping = aes(x=S1))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1*2))

#plotting the prabability of replicating given s1 true
P_S1_1<-ggplot(data=table1, mapping=aes(x=S1))+stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1*0.5),aes(colour="N2=0.5*N1"))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1),aes(colour="N2=N1"))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1*2),aes(colour="N2=2*N1"))+
  stat_function(fun=calculate_pcondtional, args=list(sampleS1=N_1, sampleS2=N_1*5),aes(colour="N2=5*N1"))+
  xlim(-13,13)


