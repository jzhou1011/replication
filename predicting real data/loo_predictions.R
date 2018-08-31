#reading in libraries
library(mvtnorm)
library(tidyverse)
# library(MASS)
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(reshape2)

#read in file 
#filename<-"./files/1_23669352_data_upbuilt_filtered_upbuilt.csv"
args = commandArgs(trailingOnly=TRUE)
filename<-args[1]
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
z_score_nom <- qnorm(0.05,lower.tail = FALSE)

sampleSizeS1<-mean(data$n.disc)
sampleSizeS2<-mean(data$n.rep)
N_1<-sampleSizeS1
N_2<-sampleSizeS2


#necessary functions 
#estimating variance of s1
MLE_s1<-function(var, temp.data){
  estimate<-1
  for(i in temp.data$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))^(1/20)
  }
  #estimate<-estimate*(pnorm(ZScore*sqrt(sampleSizeS1), mean=0, sd=sqrt(var))-pnorm((-ZScore)*sqrt(sampleSizeS1), mean=0, sd=sqrt(var)))^(M-MSig)
  estimate<-estimate*(pnorm(ZScore, mean=0, sd=sqrt(var))-pnorm((-ZScore), mean=0, sd=sqrt(var)))^((M-MSig)/20)
}

s1_est_func<-function(temp.data){
  max<-0
  maxVar<-0
  for(i in seq(from=0.1, to=20, by=.01)){
    temp<-MLE_s1(i,temp.data)
    #print(temp)
    if (temp>max) {
      max<-temp
      maxVar<-i
    }
  }
  return(maxVar)
}


#estimate var_g
expected_mean_ratio<-function(maxVar, sigma_g_2){
  sigma_g_2/maxVar
}

var_g_est_func<-function(maxVar, temp.data){
  sigma_g_estimator <- 0
  min_rms <- 10000
  var_g_est <- 0
  for (i in 1:10000){
    ratio<-expected_mean_ratio(maxVar,sigma_g_estimator)
    expected_s2 <- temp.data$s1*ratio
    cur_rms <- sqrt(sum((expected_s2-temp.data$s2)^2))
    if (cur_rms < min_rms){
      min_rms<- cur_rms
      var_g_est <- sigma_g_estimator
    }
    sigma_g_estimator <- sigma_g_estimator+0.01
  }
  return(var_g_est)
}


#estimating c2
c1_est_func<-function(maxVar, var_g_est){
  maxVar-1/N_1-var_g_est
}


#estimating sigmac2
MLE_joint_probability<-function(var_g, var_c1, var_c2, temp.data){
  
  cov_matrix=matrix(data=NA, nrow=2, ncol=2)
  cov_matrix[1,1]=var_g+var_c1+1/N_1
  cov_matrix[1,2]=var_g
  cov_matrix[2,1]=var_g
  cov_matrix[2,2]=var_g+var_c2+1/N_2
  
  mean_matrix=matrix(data=NA, nrow=2, ncol=1)
  mean_matrix[1,1]=0
  mean_matrix[2,1]=0
  
  estimate<-1
  
  vector<-(temp.data[,c(3,4)])
  prob<-dmvnorm(x=vector, mean = mean_matrix, sigma = cov_matrix, log = FALSE)
  
  for(i in prob){
    estimate<-estimate*(i^(1/50))
  }
  return(estimate)
}

c2_est_func<-function(var_g_est, c1_est, temp.data){
  max<-0
  c2_est<-0
  for(i in seq(from=0,to=100, by=0.01)){
    temp<-MLE_joint_probability(var_g_est, c1_est, i, temp.data)
    if(temp>max){
      max<-temp
      c2_est<-i
    }
  }
  return(c2_est)
}


#predicring replication rates 
calculate_pcondtional<-function(s1, var_g, var_c1, var_c2){
  mean<-(var_g*s1)/(1/N_1+var_g+var_c1)
  var<-var_g+var_c2+1/N_2-((var_g^2)/(var_g+var_c1+1/N_1))
  #var<-var/N_2
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
      p[i]<-(1.0-pnorm(ZScore_2, mean[i], sqrt(var)))
    }
    else{
      p[i]<-pnorm(-ZScore_2, mean[i], sqrt(var))
    }
  }
  return (p)
}


calculate_pcondtional_nom<-function(s1, var_g, var_c1, var_c2){
  mean<-(var_g*s1)/(1/N_1+var_g+var_c1)
  var<-var_g+var_c2+1/N_2-((var_g^2)/(var_g+var_c1+1/N_1))
  #var<-var/N_2
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
      p[i]<-(1.0-pnorm(z_score_nom, mean[i], sqrt(var)))
    }
    else{
      p[i]<-pnorm(-z_score_nom, mean[i], sqrt(var))
    }
  }
  return (p)
}


calculate_pcondtional_mean<-function(s1, var_g, var_c1, var_c2){
  mean<-(var_g*s1)/(1/N_1+var_g+var_c1)
  var<-var_g+var_c2+1/N_2-((var_g^2)/(var_g+var_c1+1/N_1))
  return (mean)
}


loo_pcondtional<-function(index){
  temp_s1<-results.data$s1[index]
  # print(temp_s1)
  temp.data<-results.data[-c(index),]
  maxVar<-s1_est_func(temp.data)
  var_g_est<-var_g_est_func(maxVar, temp.data)
  c1_est<-c1_est_func(maxVar, var_g_est)
  c2_est<-c2_est_func(var_g_est, c1_est, temp.data)
  prob<-calculate_pcondtional(temp_s1, var_g_est, c1_est, c2_est)
  #print(prob)
}





ZScore_2<-qnorm(0.05/nrow(results.data),lower.tail =FALSE)

loo_pcondtional<-function(index){
  temp_s1<-results.data$s1[index]
  #print(temp_s1)
  temp.data<-results.data[-c(index),]
  maxVar<-s1_est_func(temp.data)
  var_g_est<-var_g_est_func(maxVar, temp.data)
  c1_est<-c1_est_func(maxVar, var_g_est)
  c2_est<-c2_est_func(var_g_est, c1_est, temp.data)
  prob<-calculate_pcondtional(temp_s1, var_g_est, c1_est, c2_est)
  #print(prob)
  return(prob)
}

loo<-numeric(nrow(results.data))
for(i in 1:nrow(results.data)){
  #print(i)
  loo[i]=loo_pcondtional(i)
}

results.data$loo_prob<-loo
#results.data$loo_prob<-prob_table %>% select(-c(1))

prd_rep_cnt <- sum(results.data$loo_prob)
prd_rep_cnt<-formatC(prd_rep_cnt, width = 4, format="f")
results <- paste(as.character(prd_rep_cnt),"\n",sep="")
cat(results)


