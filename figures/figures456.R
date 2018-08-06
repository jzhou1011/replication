#libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#simulating data 
var_g<-2*.75
var_c1<-2*.25
var_c2<-2*.25
sigma_g<-sqrt(var_g)
sigma_c1<-sqrt(var_c1) #first study
sigma_c2<-sqrt(var_c2) #second study 
M<-100000

#generating lambda
lambda <- rnorm(n=M, mean=0, sd=sigma_g)
#generating C1
delta1 <- rnorm(n=M, mean=0, sd=sigma_c1)
#simulating C2
delta2 <- rnorm(n=M, mean=0, sd=sigma_c2)

#generating two test statistics with different confounders
data<-matrix(nrow=M, ncol=5)

for(i in 1:M){
  data[i,1]<-lambda[i]
  data[i,2]<-delta1[i]
  data[i,3]<-delta2[i]
  data[i,4]<-rnorm(n=1, mean=lambda[i]+delta1[i], sd=1) #s1
  data[i,5]<-rnorm(n=1, mean=lambda[i]+delta2[i], sd=1) #s2
}

results.data=as.data.frame(data)
colnames(results.data)=c("lambda", "delta1", "delta2", "s1", "s2")

true_lambda <- function(lambda){
  return(lambda)
}

#figure 4
figure4<-ggplot()+
  geom_point(data = filter(results.data,s1>5.2 | s1<(-5.2)),mapping = aes(x = s1, y = lambda,color="Significant"))+
  geom_point(data = filter(results.data,s1<5.2 & s1>(-5.2)),mapping = aes(x = s1, y = lambda,color="Not Significant"))+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  xlab("Discovery Sample Statistics")+ylab("True Effect Size")+
  labs(colour="Legend")+
  stat_function(fun=true_lambda,geom="line",aes(color="True effect size"))+
  scale_colour_manual(name='',values=c("Significant"="dodgerblue3","Not Significant"="slategray1","True effect size"="black"))
figure4
ggsave(filename="figure4.jpg")


#figure 5
figure5<-ggplot() +
  geom_point(data = filter(results.data, s1>5.2 | s1<(-5.2)), mapping = aes(x=s1, y = s2, color = "Significant"))+
  geom_point(data = filter(results.data, s1<5.2 & s1>(-5.2)), mapping = aes(x=s1, y = s2, color="Not Significant"))+
  geom_hline(yintercept=5.2, linetype="dashed", color = "black")+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  geom_hline(yintercept=-5.2, linetype="dashed", color = "black")+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Not Significant"="slategray1", "Significant"="dodgerblue3"))
figure5
ggsave(filename="figure5.jpg")


#figure 6
calculate_upperCI<-function(s1, c1, c2){
  sampleS1<-1
  sampleS2<-1
  sigma<-sigma_g
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*sigma^4)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(s1, c1, c2){
  sampleS1<-1
  sampleS2<-1
  sigma<-sigma_g
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*sigma^4)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}

calculate_mean<-function(s1,sampleS1,sampleS2, c1, c2){
  sampleS1<-1
  sampleS2<-1
  sigma<-sigma_g
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
}


figure6<-ggplot() +
  geom_point(data = filter(results.data, s1>5.2 | s1<(-5.2)), mapping = aes(x=s1, y = s2, color = "Significant"))+
  geom_point(data = filter(results.data, s1<5.2 & s1>(-5.2)), mapping = aes(x=s1, y = s2, color="Not Significant"))+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  stat_function(fun=calculate_mean, aes(color="Mean"), args=list(c1=var_c1, c2=var_c2))+
  stat_function(fun=calculate_lowerCI, linetype = 2, aes(color="Lower CI"),args=list(c1=var_c1, c2=var_c2))+
  stat_function(fun=calculate_upperCI,linetype = 2, aes(color="Upper CI"),args=list(c1=var_c1, c2=var_c2))+
  stat_function(fun=true_lambda,linetype = 2, aes(color="Equal statistics"))+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Not Significant"="slategray1", "Significant"="dodgerblue3", "Mean"="black", "Upper CI"="red", "Lower CI"="red","Equal statistics"="gray"))
figure6
ggsave(filename="figure6.jpg")





