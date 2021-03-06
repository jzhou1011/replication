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

s1_dist<-as.vector(s1_dist)
s2_dist1<-as.vector(s2_dist1)
results.data<-data.frame(s1_dist, s2_dist1)

tempData<-filter(results.data, s1_dist<5.2)
temp1<-((tempData$s1_dist)*(sqrt(N_2)))-((tempData$s2_dist1)*(sqrt(N_1)))
temp1<-temp1/(sqrt(N_1+N_2))
act_var<-var(temp1)

ratio <- ((tempData$s1_dist)*(sqrt(N_2)))/((tempData$s2_dist1)*(sqrt(N_1)))
mean(ratio)
temp2<-((tempData$s1_dist)*(sqrt(N_2)))-((tempData$s2_dist1)*(sqrt(N_1)))*mean(ratio)
temp2<-temp2/(sqrt(N_2+N_1))
mean(temp2)
var(temp2)
