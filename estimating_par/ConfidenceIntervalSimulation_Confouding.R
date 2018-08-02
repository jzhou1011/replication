#libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


#simulating data 
var_g<-.2
var_c1<-1
var_c2<-0
sigma<-sqrt(var_g)
sigma_c1<-sqrt(var_c1) #first study
sigma_c2<-sqrt(var_c2) #second study 
M<-1000#number of snps
#pValue<-0.05/M
#pValue<-(1*10^(-4))
ZScore<- qnorm(0.05/M,lower.tail =FALSE)
#ZScore<-5.2
sampleSizeS1<-100
sampleSizeS2<-50

#generating lambda
lambda <- rnorm(n=M, mean=0, sd=sigma)
#generating C1
delta1 <- rnorm(n=M, mean=0, sd=sigma_c1)
#simulating C2
delta2 <- rnorm(n=M, mean=0, sd=sigma_c2)

#generating two test statistics with different confounders
data1<-matrix(nrow=M, ncol=5)

for(i in 1:M){
  data1[i,1]<-lambda[i]
  data1[i,2]<-delta1[i]
  data1[i,3]<-delta2[i]
  data1[i,4]<-rnorm(n=1, mean=(lambda[i]+delta1[i])*sqrt(sampleSizeS1), sd=1) #s1
  data1[i,5]<-rnorm(n=1, mean=(lambda[i]+delta2[i])*sqrt(sampleSizeS2), sd=1) #s2
}

data1=as.data.frame(data1)
colnames(data1)=c("lambda", "delta1", "delta2", "s1", "s2")
#results.data<-filter(data1, s1>ZScore)



calculate_upperCI<-function(s1,sampleS1, sampleS2, c1, c2){
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*sigma^4)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(s1,sampleS1, sampleS2, c1, c2){
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*sigma^4)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}

calculate_mean<-function(s1,sampleS1,sampleS2, c1, c2){
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
}


pred_obs<-ggplot(data = data1, mapping = aes(x = s1, y = s2)) +
  geom_point()+
  stat_function(fun=calculate_lowerCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=var_c1, c2=var_c2), color="paleturquoise2")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=var_c1, c2=var_c2), color="paleturquoise2")+
  stat_function(fun=calculate_mean, args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=var_c1, c2=var_c2), color="palevioletred1")
pred_obs



#probability of s2 replicating given s1
# calculate_pcondtional<-function(s1,sampleS1, sampleS2){
#   sd_S1<-sqrt(sampleS1*sigma^2+1)
#   sd_S2<-sqrt(sampleS2*sigma^2+1)
#   mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
#   var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
#   p <- s1
#   for (i in 1:NROW(s1)){
#     if (s1[i]>0){
#       #p[i]<-(1-mean(pnorm(z_score, mean[i], sqrt(var2))))
#       p[i]<-(1-pnorm(z_score, mean[i], sqrt(var2)))
#       
#     }
#     else{
#       # p[i]<-mean(pnorm(-z_score, mean[i], sqrt(var2)))
#       p[i]<-pnorm(-z_score, mean[i], sqrt(var2))
#     }
#   }
#   return(p)
# }
