#libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 
tempSd<-sqrt(2)
sigma<-tempSd
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=tempSd)
results<-matrix(nrow=N, ncol=3)
for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i],sd=1)
}
results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "Discovery_Sample_Statistics", "Replication_Sample_Statistics")

#mean
calculate_mean<-function(s1){
  sd_S1<-sqrt(1*sigma^2+1)
  sd_S2<-sqrt(1*sigma^2+1)
  mean<-(sqrt(1)*sqrt(1)*sigma^2*s1)/(sd_S1^2)
}

calculate_upperCI<-function(s1){
  sampleS1<-1
  sampleS2<-1
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(s1){
  sampleS1<-1
  sampleS2<-1
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}


s1VSs2_4<-ggplot() +
  geom_point(data = filter(results.data, Discovery_Sample_Statistics>4 | Discovery_Sample_Statistics<(-4)), mapping = aes(x=Discovery_Sample_Statistics, y = Replication_Sample_Statistics, color = "Significant"))+
  geom_point(data = filter(results.data, Discovery_Sample_Statistics<4 & Discovery_Sample_Statistics>(-4)), mapping = aes(x=Discovery_Sample_Statistics, y = Replication_Sample_Statistics, color="Not Significant"))+
 # geom_hline(yintercept=4, linetype="dashed", color = "black")+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  #geom_hline(yintercept=-4, linetype="dashed", color = "black")+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  stat_function(fun=calculate_mean, aes(color="mean"))+
  stat_function(fun=calculate_lowerCI, linetype = 2, aes(color="Lower CI"))+
  stat_function(fun=calculate_upperCI,linetype = 2, aes(color="Upper CI"))+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Not Significant"="slategray1", "Significant"="dodgerblue3", "mean"="black", "Upper CI"="red", "Lower CI"="red"))
s1VSs2_4

#color="dodgerblue3"

ggsave(filename="Figure3.jpg")


