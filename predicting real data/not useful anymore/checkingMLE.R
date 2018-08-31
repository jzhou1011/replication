# libraries
# library(tidyverse)
# library(MASS)
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(reshape2)


#simulating data 
var_g<-2.0
var_c1<-1.0
sigma_g<-sqrt(var_g)
sigma_c1<-sqrt(var_c1) #first study
M<-1000 #number of snps
Zscore<-qnorm(.05/M,lower.tail =FALSE)

#generating lambda
lambda <- rnorm(n=M, mean=0, sd=sigma_g)
#generating C1
delta1 <- rnorm(n=M, mean=0, sd=sigma_c1)

#generating two test statistics with different confounders
data<-matrix(nrow=M, ncol=3)

for(i in 1:M){
  data[i,1]<-lambda[i]
  data[i,2]<-delta1[i]
  data[i,3]<-rnorm(n=1, mean=lambda[i]+delta1[i], sd=1) #s1
}

data=as.data.frame(data)
colnames(data)=c("lambda", "delta1", "s1")

sigS1<-filter(data, s1>Zscore)
MSig<-nrow(sigS1)

MLE<-function(var){
  estimate<-1
  for(i in sigS1$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))
  }
  estimate<-estimate*(pnorm(Zscore, mean=0, sd=sqrt(var)))^(M-MSig)
}


max<-0
maxVar<-0

for(i in seq(from=1, to=100, by=.001)){
  temp<-MLE(i)
  #print(temp)
  if (temp>max) {
    max<-temp
    maxVar<-i
  }
}