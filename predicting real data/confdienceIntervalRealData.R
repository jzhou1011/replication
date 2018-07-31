#testing on datasets from article 


#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#read in file 
#filename<-"./files/1_19820697_data_upbuilt_filtered_upbuilt.csv" #straight+too few data
#filename<-"./files/2_19820697_data_upbuilt_filtered_upbuilt.csv" #straight+too few data
#filename<-"./files/3_19820697_data_upbuilt_filtered_upbuilt.csv" #not enough sig s2
filename<-"./files/4_19820697_data_upbuilt_filtered_upbuilt.csv" #average #5 vs 12.96
#filename<-"./files/5_19820697_data_upbuilt_filtered_upbuilt.csv" #all sig #3 vs 2.997
#filename<-"./files/1_19343178_data_upbuilt_filtered_upbuilt.csv" #good #6 vs 21.16
#filename<-"./files/1_25282103_data_upbuilt_filtered_upbuilt.csv" #good #6 vs 21.16

#args = commandArgs(trailingOnly=TRUE)
#filename<-args[1]

data<-read.csv(filename, sep=",")

#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)

#needed functions and math
M=nrow(data)
var=data$trait.var[1]
#sampleSize
sigma=sqrt(var)
threshold=data$p.thresh[1]
z_score_nom <- qnorm(0.05,lower.tail = FALSE)
z_score <- qnorm(0.05/M,lower.tail =FALSE)

# sampleSizeS1=data$n.disc
# sampleSizeS2=data$n.rep
sampleSizeS1<-mean(data$n.disc)
sampleSizeS2<-mean(data$n.rep)
results.data$actual_rep = rep(0,M)

for (i in 1:M){
  # if (results.data$s2[i]>z_score | results.data$s2[i]<(-z_score))
  #   results.data$actual_rep[i] = 1
  if (results.data$s2[i]>z_score_nom && results.data$s1[i]>0)
    results.data$actual_rep[i] = 1
  if (results.data$s1[i]< 0 && results.data$s2[i]<(-z_score_nom))
    results.data$actual_rep[i] = 1
}


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



calculate_upperCI<-function(s1,sampleS1, sampleS2){
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(s1,sampleS1, sampleS2){
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}

calculate_mean<-function(s1,sampleS1,sampleS2){
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
}


pred_obs<-ggplot(data = results.data, mapping = aes(x = s1, y = s2)) +
  geom_point()+
  stat_function(fun=calculate_lowerCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2), color="paleturquoise2")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2), color="paleturquoise2")+
  stat_function(fun=calculate_mean, args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2), color="palevioletred1")
pred_obs
