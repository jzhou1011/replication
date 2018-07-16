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
sampleSizeS1=data$n.disc
sampleSizeS2=data$n.rep


#calculating condtional with different sample sizes. 
#this one is assuming same sample size
# calculate_pcondtional<-function(s1){
#   mean<-(sigma^2*s1)/(1+sigma^2)
#   var=1+((sigma^2)/(1+sigma^2))
#   p<- (1-pnorm(zScore, mean, sqrt(var)))+pnorm(-zScore, mean, sqrt(var))
#   return(p)
# }

calculate_pcondtional<-function(s1,sampleS1, sampleS2){
  mean<-((sigma^2*s1)/(1+sigma^2))*(sqrt(sampleS1/sampleS2))
  var=(1+((sigma^2)/(1+sigma^2)))*(sampleS1)^2
  p<- (1-pnorm(zScore, mean, sqrt(var)))+pnorm(-zScore, mean, sqrt(var))
  return(p)
}



#observed replication rate 
s1_sig<-nrow(filter(results.data, s1>zScore|s1<(-zScore)))
s2_sig_givens1<-nrow(filter(filter(results.data, s1>zScore), s2>zScore)) + nrow(filter(filter(results.data, s1<(-zScore)), s2<(-zScore)))
repRate=s2_sig_givens1/s1_sig

s1_sig_vector <-filter(results.data, s1>zScore|s1<(-zScore))$s1
theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector, sampleSizeS1, sampleSizeS2))/N
#theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector))/N

