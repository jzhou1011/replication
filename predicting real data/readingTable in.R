
#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

filename<-"output2_high.txt"

data<-read.csv(filename, sep=" ")
#data<-data[, c(-9)]
data<-na.omit(data)
# data$pred_c<-as.character(data$pred_c)
# data$pred_c<-as.numeric(data$pred_c)

data[2:11] <- lapply(data[2:11], as.numeric)
data$pred_c<-format(round(data$pred_c, 2), nsmall = 2)
data[2:11] <- lapply(data[2:11], as.numeric)


#our observed is wrong again... 
#data$diff<-(data$obs)-(data$pred_c)

sum(data$obs_nom)
sum(data$pred_wc_n)
sum(data$pred_c_n)
sum(data$obs)
sum(data$pred_wc)
sum(data$pred_c)

ggplot(data = data, mapping=aes(x=pred_c,y=obs))+geom_point()+
  xlab("Predicted rep count with confounding and winners curse")+
  ylab("Observed rep count")
ggplot(data = data, mapping=aes(x=pred_wc,y=obs))+geom_point()+
  xlab("Predicted rep count with winners curse")+
  ylab("Observed rep count")
