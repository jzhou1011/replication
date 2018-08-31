#predicting whether s2 replicates given s1. Different Sample Size 

#library(tidyverse)
#library(dplyr)

#read in data
#filename<-"./files/1_25282103_data_upbuilt_filtered_upbuilt.csv" #good 
#filename<-"./files/3_23118974_data_upbuilt_filtered_upbuilt.csv"

#reading in data through command line 
args = commandArgs(trailingOnly=TRUE)
filename<-args[1]
data<-read.csv(filename, sep=",")


z_score <- qnorm(0.025,lower.tail =FALSE)
M=nrow(data)
var=data$trait.var[1]
sigma=sqrt(var)

#calculating averages for scaling 
averages1=mean(data$n.disc)
averages2=mean(data$n.rep)

#find test statsitic with scaling
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-((data$beta.disc)/(data$se.disc))
results.data$s2<-((data$beta.rep)/(data$se.rep))

temp<-((results.data$s1)*(sqrt(averages2)))-(results.data$s2*(sqrt(averages1)))
temp<-temp
act_var<-var(temp)/(averages1+averages2)

#act_var<-act_var/(sqrt(averages2+averages1))
act_var_f <- formatC(act_var, width = 4, format="fg")
results <-as.character(act_var_f)
cat(results)

