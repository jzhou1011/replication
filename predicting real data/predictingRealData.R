#testing on datasets from article 


#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


#read in file 
#filename<-"19820697_data_upbuilt_filtered_upbuilt.csv" #not good 
#filename<-"19343178_data_upbuilt_filtered_upbuilt.csv" #good
#filename<-"25282103_data_upbuilt_filtered_upbuilt.csv" #good 
#filename<-"19557161_data_upbuilt_filtered_upbuilt.csv" #good 
#filename<-"20010834_data_upbuilt_filtered_upbuilt.csv" #good #not bad
filename<-"20887962_data_upbuilt_filtered_upbuilt.csv" #not good 
#filename<-"21738478_data_upbuilt_filtered_upbuilt.csv" #good #closer when using ratio 
#filename<-"21909110_data_upbuilt_filtered_upbuilt.csv" #good ratio and not is the same and its ok
#filename<-"21947420_data_upbuilt_filtered_upbuilt.csv" #good will be off but shoudl be good? 
#filename<-"21998595_data_upbuilt_filtered_upbuilt.csv" #good ratio is good 
#filename<-"22021425_data_upbuilt_filtered_upbuilt.csv" #good ratio is good
#filename<-"22267201_data_upbuilt_filtered_upbuilt.csv" #good #very good 
#filename<-"22504420_data_upbuilt_filtered_upbuilt.csv" #good #not working
#filename<-"23118974_data_upbuilt_filtered_upbuilt.csv" #mostly not sigfnicant
#filename<-"23263486_data_upbuilt_filtered_upbuilt.csv" #kinda good
#filename<-"23669352_data_upbuilt_filtered_upbuilt.csv" #idk
#filename<-"23658558_data_upbuilt_filtered_upbuilt.csv" #hm
#filename<-"24430505_data_upbuilt_filtered_upbuilt.csv" #hm idk
#filename<-"24952745_data_upbuilt_filtered_upbuilt.csv" #accetable 
#filename<-"25035420_data_upbuilt_filtered_upbuilt.csv" #straight
#filename<-"22267201_data_upbuilt_filtered_upbuilt.csv"
data<-read.csv(filename, sep=" ")

#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)

checkingDist<-ggplot(data = filter(results.data), mapping = aes(x = s1, y = s2)) +
  geom_point()+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))

checkingDist

z_score <- qnorm(0.025,lower.tail =FALSE)


#needed functions and math
M=nrow(data)
var=data$trait.var[1]
#sampleSize
sigma=sqrt(var)
threshold=data$p.thresh[1]

# sampleSizeS1=data$n.disc
# sampleSizeS2=data$n.rep
sampleSizeS1=1*scalingFactor
sampleSizeS2=(mean(data$n.rep)/mean(data$n.disc))*scalingFactor
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
     #p[i]<-(1-mean(pnorm(z_score, mean[i], sqrt(var2))))
      p[i]<-(1-pnorm(z_score, mean[i], sqrt(var2)))
      
    }
    else{
     # p[i]<-mean(pnorm(-z_score, mean[i], sqrt(var2)))
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


