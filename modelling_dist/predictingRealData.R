#testing on datasets from article 


#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


#read in file 
fileName<-"22021425_data_upbuilt_filtered_upbuilt.csv"
data<-read.csv(fileName, sep=" ")

#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc-data$trait.mean)/(data$se.disc)
results.data$s2<-(data$beta.rep-data$trait.mean)/(data$se.rep)


#needed functions and math
N=nrow(data)
var=data$trait.var[1]
#sampleSize
sigma=sqrt(var)
threshold=data$p.thresh[1]
zScore=qnorm(threshold, lower.tail = FALSE)
sampleSizeS1=1
sampleSizeS2=data$n.rep/data$n.disc


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
  mean<-(sqrt(sampleS1*sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  p<- (1-pnorm(zScore, mean, sqrt(var2)))+pnorm(-zScore, mean, sqrt(var2))
  return(p)
}

#observed replication rate 
s1_sig<-nrow(filter(results.data, s1>zScore|s1<(-zScore)))
s2_sig_givens1<-nrow(filter(filter(results.data, s1>zScore), s2>zScore)) + nrow(filter(filter(results.data, s1<(-zScore)), s2<(-zScore)))
repRate=s2_sig_givens1/s1_sig

s1_sig_vector <-filter(results.data, s1>zScore|s1<(-zScore))$s1
#theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector, sampleSizeS1, sampleSizeS2))/N
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector,sampleSizeS1, sampleSizeS2))/N

