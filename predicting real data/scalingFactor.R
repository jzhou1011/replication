#predicting whether s2 replicates given s1. Different Sample Size 

library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#reading in data
filename<-"25035420_data_upbuilt_filtered_upbuilt.csv"
data<-read.csv(filename, sep=" ")


z_score <- qnorm(0.025,lower.tail =FALSE)

#needed functions and math
M=nrow(data)
var=data$trait.var[1]

#sampleSize
sigma=sqrt(var)
threshold=data$p.thresh[1]

sampleSizeS1=data$n.disc/data$n.disc
sampleSizeS2=data$n.rep/data$n.disc

#calculating averages for scaling 
averages1=mean(data$n.disc)
averages2=mean(data$n.rep)

#find test statsitic with scaling
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(1/sqrt(averages1))*(data$beta.disc)/(data$se.disc)
results.data$s2<-(1/sqrt(averages2))*(data$beta.rep)/(data$se.rep)

theo_var<-(1/averages1)+(1/averages2)
act_var<-var((results.data$s1)-(results.data$s2))
scalingFactor<-theo_var/act_var
