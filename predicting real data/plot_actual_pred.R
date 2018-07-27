library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(reshape2)


filename <- "output2.txt"
data<-read.table(filename, sep=" ",header=TRUE)

sum(data$M)
sum(data$obs)
sum(data$pred)

pred_obs<-ggplot(data = data, mapping = aes(x = obs, y = pred)) +geom_point()+xlim(0,100)+ylim(0,100)
pred_obs

pred_obs_filtered<-ggplot(data =filter(filter(data,var<3), var > 0.6), mapping = aes(x = obs, y = pred)) +geom_point()+xlim(0,100)+ylim(0,100)+stat_function(mapping=)
pred_obs_filtered2

pred_obs_filtered<-ggplot(data = filter(filter(filter(data,var<3), var > 0.6),M>10), mapping = aes(x = obs, y = pred)) +geom_point()+xlim(0,100)+ylim(0,100)+stat_function(mapping=)
pred_obs_filtered