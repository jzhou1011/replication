#libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 
tempSd<-sqrt(2)
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=tempSd)
results<-matrix(nrow=N, ncol=3)
for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i],sd=1)
}
results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

s1VSs2_4<-ggplot(data = filter(results.data, s1>4 | s1<(-4)), mapping = aes(x = s1, y = s2)) +
  geom_point()+geom_hline(yintercept=4, linetype="dashed", color = "red")+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  geom_hline(yintercept=-4, linetype="dashed", color = "red")

ggsave(filename="s1_s2_replication_4.jpg")
