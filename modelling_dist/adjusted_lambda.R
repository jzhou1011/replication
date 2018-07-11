library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#pulling lamda from normal distrubtion 
sigma<-sqrt(2)
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=sigma)

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

true_lambda <- function(lambda){
  return(lambda)
}

compute_mean <- function(s1){
  sigma^2*s1/(1+sigma^2)
}

results.data$pred_lambda <- compute_mean(results.data$s1)

s1_val <- results.data[c("lamda","s1")]
s1_val$grouping = rep("s1",10000)
colnames(s1_val) = c("lamda","value")
pred_val <- results.data[c("lamda","pred_lambda")]
pred_val$grouping = rep("pred",10000)
colnames(pred_val) = c("lamda","value")
result.sp.data <- rbind(s1_val,pred_val)
colnames(result.sp.data) <- c("lamda","value","grouping")

trueVSpred<-ggplot(data = result.sp.data, mapping = aes(x = lamda, y =value, colour=(result.sp.data$grouping))) +geom_point()+scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+ stat_function(fun=true_lambda,geom="line",colour="blue")
#trueVSpred<-ggplot(data = results.data, mapping = aes(x = lamda, y = s1_pred)) +geom_point()+scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+ stat_function(fun=true_lambda,geom="line",colour="blue")

