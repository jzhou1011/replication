# library(tidyverse)
# library(dplyr)
# #install.packages("mvtnorm")
# library(mvtnorm)
# 

var_g<-2
var_c1<-2
var_c2<-4
M<-1000
N_1<-2
N_2<-3
zscore1<-qnorm(0.05/M,lower.tail = FALSE)

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
s1_sig <- filter(results.data,s1_dist>zscore1)
num_s1 <-nrow(s1_sig)
zscore2<-qnorm(0.05/num_s1,lower.tail = FALSE)
s1_s2_sig<-filter(s1_sig,s2_dist>zscore2)

#s1 variance
MLE<-function(var){
  estimate<-choose(M,num_s1)
  for(i in s1_sig$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))
  }
  estimate<-estimate*(pnorm(zscore1, mean=0, sd=sqrt(var)))^(M-num_s1)
}

max<-0
maxVar<-0

for(i in seq(from=0.001, to=50, by=.001)){
  temp<-MLE(i)
  #print(temp)
  if (temp>max) {
    max<-temp
    maxVar<-i
  }
}

#sigma_g
#sigma_g <- mean(s1_sig$s2_dist/s1_sig$s1_dist)*maxVar/sqrt(N_1*N_2)
var_g_est1 <- mean(s1_sig$s2_dist)/mean(s1_sig$s1_dist)*maxVar/sqrt(N_1*N_2)


#first method to estimate c1
c1_est1<-(maxVar-1-var_g_est1*(N_1))/(N_1)
#estimating c2
#this is not accuraute and we must fine another way to do this. 
# var_1<-((s1_sig$s1_dist)*(sqrt(N_2)))-(s1_sig$s2_dist*(sqrt(N_1)))
# var_1<-var(var_1)/(N_1+N_2)
# 
# expected<-(N_2*(1+N_1*var_c1)+N_1*(1+N_2*var_c2))/(N_1+N_2)

#second method to estimate sigma_g
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

#estimating sigmac2

#using corrected s1 

expected_mean_lambda<-function(s1, c1, var_g){
  (sqrt(N_1*N_2)*var_g*s1)/(1+N_1*var_g+N_1*c1)
}


s1_sig$predLambda<-expected_mean_lambda(s1_sig$s1_dist, var_c1, var_g)

  
MLE_joint_probability<-function(var_g, var_c1, var_c2){
  cov_matrix=matrix(data=NA, nrow=2, ncol=2)
  cov_matrix[1,1]=N_1*var_g+N_1*var_c1+1
  cov_matrix[1,2]=sqrt(N_1*N_2)*var_g
  cov_matrix[2,1]=sqrt(N_1*N_2)*var_g
  cov_matrix[2,2]=N_2*var_g+N_2*var_c2+1
  
  mean_matrix=matrix(data=NA, nrow=2, ncol=1)
  mean_matrix[1,1]=0
  mean_matrix[2,1]=0
  
  estimate<-choose(M,num_s1)
  
  vector<-(s1_sig %>% select(s1_dist, s2_dist))
  prob<-dmvnorm(x=vector, mean = mean_matrix, sigma = cov_matrix, log = FALSE)
  
  for(i in prob){
    estimate<-estimate*i
  }
  estimate*pnorm(zscore1, mean=0, sd=sqrt(var_g))^(M-num_s1)

}


max<-0
c2_est<-0
for(i in seq(from=0,to=10, by=0.001)){
  temp<-MLE_joint_probability(var_g, var_c1, i)
  if(temp>max){
    max<-temp
    c2_est<-i
  }
}




