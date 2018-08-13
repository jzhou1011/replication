
#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

filename<-"output2.txt"

data<-read.csv(filename, sep=" ")
data<-data[, c(-9)]
data<-na.omit(data)
# data$pred_c<-as.character(data$pred_c)
# data$pred_c<-as.numeric(data$pred_c)

data[2:8] <- lapply(data[2:8], as.numeric)
data$pred_c<-format(round(data$pred_c, 2), nsmall = 2)
data[2:8] <- lapply(data[2:8], as.numeric)


#our observed is wrong again... 
data$diff<-(data$obs)-(data$pred_c)
