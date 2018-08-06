library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

tempSd<-sqrt(2)
N=10000
sampleSize=10000
lambda <- rnorm(n=N, mean=0, sd=tempSd)

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lambda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lambda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lambda", "s1", "s2")

true_lambda <- function(lambda){
  return(lambda)
}


s1VSlambda<-ggplot()+
  geom_point(data = filter(results.data,s1>4 | s1<(-4)),mapping = aes(x = s1, y = lambda,color="Significant"))+
  geom_point(data = filter(results.data,s1<4 & s1>(-4)),mapping = aes(x = s1, y = lambda,color="Not Significant"))+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  xlab("Discovery Sample Statistics")+ylab("True Effect Size")+
  labs(colour="Legend")+
  stat_function(fun=true_lambda,geom="line",aes(color="True effect size"))+
  scale_colour_manual(name='',values=c("Significant"="dodgerblue3","Not Significant"="slategray1","True effect size"="black"))
s1VSlambda
ggsave(filename="figure1.jpg")
