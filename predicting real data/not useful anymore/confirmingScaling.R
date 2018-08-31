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
N_1=100
N_2=200
#N_1=10000
z_score=5.2


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

for (i in 1:N){
  s1_dist[i]<-(1/sqrt(N_1))*rnorm(n=1,mean=lamda[i]*sqrt(N_1),sd=1)
  s2_dist1[i]<-(1/sqrt(N_2))*rnorm(n=1,mean=lamda[i]*sqrt(N_2),sd=1)
}

s1s2<-(s1_dist)-(s2_dist1)
var(s1s2)
(1/N_1)+(1/N_2)