#predicting whether s2 replicates given s1. Different Sample Size 

library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 

M=1000000
N_1=2000
N_2=1000
z_score=5.2


var1<-2
var2<-100
var3<-1000



calculate_pcondtional<-function(s1,sampleS1, sampleS2, var){
  sigma<-sqrt(var)
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var_temp<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
      p[i]<-(1-pnorm(z_score, mean[i], sqrt(var_temp)))
    }
    else{
      p[i]<-pnorm(-z_score, mean[i], sqrt(var_temp))
    }
  }
  return(p)
}




probabilties_conditional<-ggplot(data.frame(s1 = c(-8, 8)), aes(s1))+stat_function(fun=calculate_pcondtional, args=list(var=var1, sampleS1=N_1, sampleS2=N_1*0.5), aes(colour="var1"))+
  stat_function(fun=calculate_pcondtional, args=list(var=var2, sampleS1=N_1, sampleS2=N_1*0.5), aes(colour="var2"))+
  stat_function(fun=calculate_pcondtional, args=list(var=var3, sampleS1=N_1, sampleS2=N_1*0.5), aes(colour="var3"))
  
probabilties_conditional

