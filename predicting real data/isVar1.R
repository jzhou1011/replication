#predicting whether s2 replicates given s1. Different Sample Size 

library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#reading in data
filename<-"1_24952745_data_upbuilt_filtered_upbuilt.csv"
data<-read.csv(filename, sep=",")


z_score <- qnorm(0.025,lower.tail =FALSE)
M=nrow(data)
var=data$trait.var[1]
sigma=sqrt(var)
threshold=data$p.thresh[1]


#calculating averages for scaling 
averages1=mean(data$n.disc)
averages2=mean(data$n.rep)

#find test statsitic with scaling
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-((data$beta.disc)/(data$se.disc))*(sqrt(averages2))
results.data$s2<-((data$beta.rep)/(data$se.rep))*(sqrt(averages1))

temp<-results.data$s1-results.data$s2
temp<-temp/(sqrt(averages1+averages2))
act_var<-var(temp)

#act_var<-act_var/(sqrt(averages2+averages1))


#eskins way 


