#predicting whether s2 replicates given s1. Different Sample Size 

library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 
var<-2
sigma<-sqrt(var)
M=100000
N_1=1
N_2=1
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

lamda <- rnorm(n=M, mean=0, sd=sigma)

s1_dist<-matrix(nrow=M, ncol=1)
s2_dist1<-matrix(nrow=M, ncol=1)

for (i in 1:M){
  s1_dist[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_1),sd=1)
  s2_dist1[i]<-rnorm(n=1,mean=lamda[i]*sqrt(N_2),sd=1)
}

s1_dist<-as.vector(s1_dist)
s2_dist1<-as.vector(s2_dist1)

results.data<-data.frame(s1_dist, s2_dist1)
#calculating variance normally to check to see if its 1 
# temp1<-((s1_dist)*(sqrt(N_2)))-((s2_dist1)*(sqrt(N_1)))
# temp1<-temp1/(sqrt(N_1+N_2))
# act_var<-var(temp1)

#s1_dist_copy<-subset(s1_dist, s1_dist>5.2 | s1_dist<5.2)
#s2_dist_copy<-subset(s2_dist1, s2_dist1>5.2 | s2_dist1<5.2)

#using signifcant s1 and s1
# temp1_c<-((s1_dist_copy)*(sqrt(N_2)))-((s2_dist_copy)*(sqrt(N_1)))
# temp1_c<-temp1/(sqrt(N_1+N_2))
# act_var_c<-var(temp1)


variance <- function(x) {
  n <- length(x)
  #m <- mean(x)
  m<-0
  (1/(n - 1)) * sum((x - m)^2)
}

#calcualting variance for only signifcant s1 
tempData<-filter(results.data, s1_dist>5.2)
temp1<-((tempData$s1_dist)*(sqrt(N_2)))-((tempData$s2_dist1)*(sqrt(N_1)))
temp1<-temp1/(sqrt(N_1+N_2))
act_var<-variance(temp1)

#checking winners curse 
s1_s2<-ggplot(data = tempData, mapping = aes(x = s1_dist, y = s2_dist1,colour="blue")) +geom_point()
s1_s2

mean(tempData$s2_dist1)
mean(tempData$s1_dist)