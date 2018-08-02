#simulating data with confounding 
#attempting to estimate the confouding with only the signifcant values 

# #libraries
# library(tidyverse)
# library(MASS)
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(reshape2)


#simulating data 
var_g<-0.82
var_c1<-10.0
var_c2<-2.0
sigma_g<-sqrt(var_g)
sigma_c1<-sqrt(var_c1) #first study
sigma_c2<-sqrt(var_c2) #second study 
M<-32#number of snps
#pValue<-0.05/M
pValue<-(1*10^(-4))
ZScore<- qnorm(0.05/M,lower.tail =FALSE)
#ZScore<-5.2

#generating lambda
lambda <- rnorm(n=M, mean=0, sd=sigma_g)
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
  data1[i,4]<-rnorm(n=1, mean=lambda[i]+delta1[i], sd=1) #s1
  data1[i,5]<-rnorm(n=1, mean=lambda[i]+delta2[i], sd=1) #s2
}

data1=as.data.frame(data1)
colnames(data1)=c("lambda", "delta1", "delta2", "s1", "s2")
data1$avg<-(data1$s1+data1$s2)*0.5
s1SigData<-filter(data1, s1>ZScore | s1<(-ZScore))


s1s2<-ggplot(data=data1, mapping = aes(x=s1, y=s2))+geom_point()+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))

#s1s2

s1Sigs2<-ggplot(data=s1SigData, mapping = aes(x=s1, y=s2))+geom_point()+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))

s1Sigs2


#calculating s2 given s1
calculate_pcondtional<-function(s1){
  mean<-(sigma_g^2*s1)/(1+sigma_g^2+sigma_c1^2)
  var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  p<- (1-pnorm(5.2, mean, sqrt(var)))+pnorm(-5.2, mean, sqrt(var))
  return(p)
}


#estimating sigma g from having two confounders 
#variance of (s1+s2)-(s1-s2)
# sigma_g_est=(var(data$s1+data$s2)-var(data$s1-data$s2))*(1/4)
# var_c1_est=var(data$s1)-1-sigma_g_est
# var_c2_est=var(data$s2)-1-sigma_g_est


