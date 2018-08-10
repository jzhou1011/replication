#testing on datasets from article 


#reading in libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#read in file 
filename<-"./files/4_19820697_data_upbuilt_filtered_upbuilt.csv" #average #5 vs 12.96
#args = commandArgs(trailingOnly=TRUE)
#filename<-args[1]
data<-read.csv(filename, sep=",")

#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)

thresh<-data$p.thresh[1]
ZScore<-qnorm(thresh,lower.tail =FALSE) #Zscore they used in orignal study s1. 
M<-0.05/thresh
MSig<-nrow(data)
traitVar<-data$trait.var[1]

# sampleSizeS1=data$n.disc
# sampleSizeS2=data$n.rep
sampleSizeS1<-mean(data$n.disc)
sampleSizeS2<-mean(data$n.rep)
N_1<-sampleSizeS1
N_2<-sampleSizeS2

#estimating variance of s1
MLE<-function(var){
  estimate<-choose(M,MSig)
  for(i in results.data$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))
  }
  estimate<-estimate*(pnorm(ZScore*sqrt(sampleSizeS1), mean=0, sd=sqrt(var))-pnorm((-ZScore)*sqrt(sampleSizeS1), mean=0, sd=sqrt(var)))^(M-MSig)
}

max<-0
maxVar<-0
for(i in seq(from=0.1, to=10*sampleSizeS1, by=.1)){
  temp<-MLE(i)
  #print(temp)
  if (temp>max) {
    max<-temp
    maxVar<-i
  }
}

#first method to estimate var_g
expected_mean_ratio<-function(sigma_g_2){
  sqrt(N_1*N_2)*sigma_g_2/maxVar
}
sigma_g_estimator <- 0
min_rms <- 10000
var_g_est2 <- 0
for (i in 1:10000){
  ratio<-expected_mean_ratio(sigma_g_estimator)
  expected_s2 <- results.data$s1*ratio
  cur_rms <- sqrt(sum((expected_s2-results.data$s2)^2))
  if (cur_rms < min_rms){
    min_rms<- cur_rms
    var_g_est2 <- sigma_g_estimator
  }
  sigma_g_estimator <- sigma_g_estimator+0.000001
}

#var_g_1 <- mean(results.data$s2/results.data$s1)*maxVar/sqrt(sampleSizeS1*sampleSizeS2)
#estimating c2
c1_est2<-(maxVar-1-var_g_est2*(N_1))/(N_1)


#estimating sigmac2
MLE_joint_probability<-function(var_g, var_c1, var_c2){
  cov_matrix=matrix(data=NA, nrow=2, ncol=2)
  cov_matrix[1,1]=N_1*var_g+N_1*var_c1+1
  cov_matrix[1,2]=sqrt(N_1*N_2)*var_g
  cov_matrix[2,1]=sqrt(N_1*N_2)*var_g
  cov_matrix[2,2]=N_2*var_g+N_2*var_c2+1
  
  mean_matrix=matrix(data=NA, nrow=2, ncol=1)
  mean_matrix[1,1]=0
  mean_matrix[2,1]=0
  
  estimate<-choose(M,MSig)
  
  vector<-(results.data[,c(3,4)])
  prob<-dmvnorm(x=vector, mean = mean_matrix, sigma = cov_matrix, log = FALSE)
  
  for(i in prob){
    estimate<-estimate*i
  }
  return(estimate)
}

max<-0
c2_est<-0
for(i in seq(from=0,to=10, by=0.0001)){
  temp<-MLE_joint_probability(var_g_est2, c1_est2, i)
  if(temp>max){
    max<-temp
    c2_est<-i
  }
}

#checking distribution

calculate_upperCI<-function(s1,sampleS1, sampleS2, var_g, c1, c2){
  sd_S1<-sqrt(sampleS1*var_g+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*var_g+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*var_g*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*var_g^2)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(s1,sampleS1, sampleS2, var_g, c1, c2){
  sd_S1<-sqrt(sampleS1*var_g+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*var_g+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*var_g*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*var_g^2)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}

calculate_mean<-function(s1,sampleS1,sampleS2, var_g, c1, c2){
  sd_S1<-sqrt(sampleS1*var_g+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*var_g+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*var_g*s1)/(sd_S1^2)
}


checkingDist<-ggplot(data = filter(results.data), mapping = aes(x = s1, y = s2)) +
  geom_point()+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
  stat_function(fun=calculate_lowerCI,args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2, var_g=var_g_est2, c1=c1_est2, c2=c2_est),linetype = 2, color="red")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2, var_g=var_g_est2, c1=c1_est2, c2=c2_est),linetype = 2, mapping =aes(color="Confidence Interval"))+
  stat_function(fun=calculate_mean, args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2, var_g=var_g_est2, c1=c1_est2, c2=c2_est), mapping=aes(color="Mean"))

checkingDist

#Predicting replication rate 
ZScore_2<-qnorm(0.05/nrow(results.data),lower.tail =FALSE)

calculate_pcondtional<-function(s1, var_g, var_c1, var_c2){
  mean<-(sqrt(N_1*N_2)*var_g*s1)/(1+N_1*var_g+N_1*var_c1)
  #var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  var<-(N_2*var_g)+(N_2*var_c2)+1-((N_1*N_2*var_g^2)/(N_1*var_g+N_1*var_c1+1))
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
      p[i]<-(1-pnorm(ZScore_2, mean[i], sqrt(var)))
    }
    else{
      p[i]<-pnorm(-ZScore_2, mean[i], sqrt(var))
    }
  }
  return (p)
}

theo_rep <- sum(calculate_pcondtional(results.data$s1, var_g_est2, c1_est2, c2_est))/nrow(results.data)

s1_s2_sig<-results.data %>% filter(s2>ZScore_2 | s2<(-ZScore_2))
repRate=nrow(s1_s2_sig)/nrow(results.data)


