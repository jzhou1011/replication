library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

sigma<-sqrt(2)
M<-100000
sampleSize=10000
lambda <- rnorm(n=M, mean=0, sd=sigma)

results<-matrix(nrow=M, ncol=3)

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

for(i in 1:M){
  results[i,1] = lambda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lambda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lambda", "s1", "s2")

true_lambda <- function(lambda){
  return(lambda)
}

s1VSlambda<-ggplot()+
  geom_point(data = filter(results.data,s1>5.2 | s1<(-5.2)),mapping = aes(x = s1, y = lambda,color="Significant"))+
  geom_point(data = filter(results.data,s1<5.2 & s1>(-5.2)),mapping = aes(x = s1, y = lambda,color="Not Significant"))+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  xlab("Discovery Sample Statistics")+ylab("True Effect Size")+
  labs(colour="Legend")+
  stat_function(fun=true_lambda,geom="line",aes(color="True effect size"))+
  scale_colour_manual(name='',values=c("Significant"="dodgerblue3","Not Significant"="slategray1","True effect size"="black"))
s1VSlambda
ggsave(filename="figure1.jpg")

s1VSs2_4<-ggplot() +
  geom_point(data = filter(results.data, s1>5.2 | s1<(-5.2)), mapping = aes(x=s1, y = s2, color = "Significant"))+
  geom_point(data = filter(results.data, s1<5.2 & s1>(-5.2)), mapping = aes(x=s1, y = s2, color="Not Significant"))+
  geom_hline(yintercept=4, linetype="dashed", color = "black")+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  geom_hline(yintercept=-4, linetype="dashed", color = "black")+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Not Significant"="slategray1", "Significant"="dodgerblue3"))
s1VSs2_4
ggsave(filename="figure2.jpg")

figure3<-ggplot() +
  geom_point(data = filter(results.data, s1>5.2 | s1<(-5.2)), mapping = aes(x=s1, y = s2, color = "Significant"))+
  geom_point(data = filter(results.data, s1<5.2 & s1>(-5.2)), mapping = aes(x=s1, y = s2, color="Not Significant"))+
  # geom_hline(yintercept=4, linetype="dashed", color = "black")+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  #geom_hline(yintercept=-4, linetype="dashed", color = "black")+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  stat_function(fun=calculate_mean, aes(color="Mean"))+
  stat_function(fun=calculate_lowerCI, linetype = 2, aes(color="Lower CI"))+
  stat_function(fun=calculate_upperCI,linetype = 2, aes(color="Upper CI"))+
  stat_function(fun=true_lambda,linetype = 2, aes(color="Actual Sample Size"))+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Not Significant"="slategray1", "Significant"="dodgerblue3", "Mean"="black", "Upper CI"="red", "Lower CI"="red","Actual Sample Size"="black"))
figure3
ggsave(filename="figure3.jpg")