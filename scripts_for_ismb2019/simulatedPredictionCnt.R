library(tidyverse)
library(dplyr)
#install.packages("mvtnorm")
library(mvtnorm)

M<-1000
N_1<-2
N_2<-3
zscore1<-qnorm(0.025/M,lower.tail = FALSE)

MLE<-function(var,s1_sig,num_s1){
  estimate<-1
  for(i in s1_sig$s1){
    estimate<-estimate*(dnorm(i, mean=0, sd=sqrt(var))^(1/50))
  }
  estimate<-estimate*((pnorm(zscore1, mean=0, sd=sqrt(var))-pnorm((-zscore1), mean=0, sd=sqrt(var)))^((M-num_s1)/50))
}

MLE_joint_probability<-function(var_g, var_c1, var_c2,N_1,N_2,s1_sig,num_s1){
  cov_matrix=matrix(data=NA, nrow=2, ncol=2)
  cov_matrix[1,1]=N_1*var_g+N_1*var_c1+1
  cov_matrix[1,2]=sqrt(N_1*N_2)*var_g
  cov_matrix[2,1]=sqrt(N_1*N_2)*var_g
  cov_matrix[2,2]=N_2*var_g+N_2*var_c2+1
  
  mean_matrix=matrix(data=NA, nrow=2, ncol=1)
  mean_matrix[1,1]=0
  mean_matrix[2,1]=0
  
  #estimate<-choose(M,num_s1)
  estimate<-1
  
  #vector<-(s1_sig %>% select(s1_dist, s2_dist))
  vector<-(s1_sig[,c(2,3)])
  prob<-dmvnorm(x=vector, mean = mean_matrix, sigma = cov_matrix, log = FALSE)
  
  for(i in prob){
    estimate<-estimate*(i^(1/20))
  }
  estimate*pnorm(zscore1, mean=0, sd=sqrt(var_g))^((M-num_s1)/20)
  
}

calculate_pcondtional<-function(s1, var_g, var_c1, var_c2,zscore2){
  mean<-(sqrt(N_2*N_1)*var_g*s1)/(1+var_g*N_1+var_c1*N_1)
  var<-N_2*var_g+N_2*var_c1+1-((N_2*N_1*var_g^2)/(N_1*var_g+N_1*var_c1+1))
  #var<-var/N_2
  p <- s1
  for (i in 1:NROW(s1)){
    p[i]<- (1.0-pnorm(zscore2, mean[i], sqrt(var)))+pnorm(-zscore2, mean[i], sqrt(var))
    # if (s1[i]>0){
    #   p[i]<-(1.0-pnorm(ZScore_2, mean[i], sqrt(var)))
    # }
    # else{
    #   p[i]<-pnorm(-ZScore_2, mean[i], sqrt(var))
    # }
  }
  return (p)
}

actualAndPredCnt<-function(var_g,var_c1,var_c2){
  
  lambda<-rnorm(n=M, mean=0, sd=sqrt(var_g))
  s1_dist<-as.vector(matrix(nrow=M, ncol=1))
  s2_dist<-as.vector(matrix(nrow=M, ncol=1))
  delta1<-rnorm(n=M, mean=0, sd=sqrt(var_c1))
  delta2<-rnorm(n=M, mean=0, sd=sqrt(var_c2))
  
  for (i in 1:M){
    s1_dist[i]<-rnorm(n=1,mean=sqrt(N_1)*(lambda[i]+delta1[i]),sd=1)
    s2_dist[i]<-rnorm(n=1,mean=sqrt(N_2)*(lambda[i]+delta2[i]),sd=1)
  }
  
  results.data<-data.frame(lambda,s1_dist, s2_dist)
  s1_sig <- filter(results.data,s1_dist>zscore1 | s1_dist <(-zscore1))
  num_s1 <-nrow(s1_sig)
  zscore2<-qnorm(0.025/num_s1,lower.tail = FALSE)
  s1_s2_sig<-filter(s1_sig,s2_dist>zscore2 | s2_dist< (-zscore2))
  
  max<-0
  maxVar<-0
  
  for(i in seq(from=0.001, to=50, by=.001)){
    temp<-MLE(i,s1_sig,num_s1)
    #print(temp)
    if (temp>max) {
      max<-temp
      maxVar<-i
    }
  }
  
  expected_mean_ratio<-function(sigma_g_2){
    sqrt(N_1*N_2)*sigma_g_2/maxVar
  }
  sigma_g_estimator <- 0
  min_rms <- 10000
  var_g_est2 <- 0
  for (i in 1:10000){
    ratio<-expected_mean_ratio(sigma_g_estimator)
    expected_s2 <- s1_sig$s1_dist*ratio
    cur_rms <- sqrt(sum((expected_s2-s1_sig$s2_dist)^2))
    if (cur_rms < min_rms){
      min_rms<- cur_rms
      var_g_est2 <- sigma_g_estimator
    }
    sigma_g_estimator <- sigma_g_estimator+0.001
  }
  
  c1_est2<-(maxVar-1-var_g_est2*(N_1))/(N_1)
  
  max<-0
  c2_est<-0
  for(i in seq(from=0,to=10, by=0.001)){
    temp<-MLE_joint_probability(var_g_est2, c1_est2, i,N_1,N_2,s1_sig,num_s1)
    if(temp>max){
      max<-temp
      c2_est<-i
    }
  }
  
  probs <- calculate_pcondtional(s1_sig$s1_dist,var_g_est2,c1_est2,c2_est,zscore2)
  predRepRate <- sum(probs)
  
  probsWC <- calculate_pcondtional(s1_sig$s1_dist,(maxVar-1)/N_1,0,0,zscore2)
  predRepRateWC <- sum(probsWC)
  
  return (c(nrow(s1_s2_sig),predRepRate,var_g_est2,c1_est2,c2_est,(maxVar-1)/N_1,predRepRateWC))
}

actualAndPredCnt(1,1,3)
