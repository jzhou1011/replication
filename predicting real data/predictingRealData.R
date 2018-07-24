#testing on datasets from article 


#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


#read in file 
fileName<-"19820697_data_upbuilt_filtered_upbuilt.csv"
#fileName<-"19343178_data_upbuilt_filtered_upbuilt.csv"
#fileName<-"25282103_data_upbuilt_filtered_upbuilt.csv"
data<-read.csv(fileName, sep=" ")

#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)

z_score <- qnorm(0.025,lower.tail =FALSE)


#needed functions and math
M=nrow(data)
var=data$trait.var[1]
#sampleSize
sigma=sqrt(var)
threshold=data$p.thresh[1]

# sampleSizeS1=data$n.disc
# sampleSizeS2=data$n.rep
sampleSizeS1=1*sqrt(scalingFactor)
sampleSizeS2=(data$n.rep/data$n.disc)*sqrt(scalingFactor)
results.data$actual_rep = rep(0,M)

for (i in 1:M){
  if (results.data$s2[i]>z_score | results.data$s2[i]<(-z_score))
    results.data$actual_rep[i] = 1
}
#calculating condtional with different sample sizes.
#this one is assuming same sample size
# calculate_pcondtional<-function(s1){
#   mean<-(sigma^2*s1)/(1+sigma^2)
#   var2=1+((sigma^2)/(1+sigma^2))
#   p<- (1-pnorm(zScore, mean, sqrt(var2)))+pnorm(-zScore, mean, sqrt(var2))
#   return(p)
# }

#probability of s2 replicating given s1
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


#observed replication rate 
# s1_sig<-nrow(filter(results.data, s1>zScore|s1<(-zScore)))
# s2_sig_givens1<-nrow(filter(filter(results.data, s1>zScore), s2>zScore)) + nrow(filter(filter(results.data, s1<(-zScore)), s2<(-zScore)))
# repRate=s2_sig_givens1/nrow(results.data)

#observed rep count
obs_rep_cnt <- sum(results.data$actual_rep)

#theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector, sampleSizeS1, sampleSizeS2))/N
# theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector,sampleSizeS1, sampleSizeS2))/N
results.data$pred_prob = calculate_pcondtional(results.data$s1,sampleSizeS1, sampleSizeS2)
prd_rep_cnt <- sum(calculate_pcondtional(results.data$s1,sampleSizeS1, sampleSizeS2))


