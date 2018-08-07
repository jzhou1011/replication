library(tidyverse)
library(dplyr)


var_g<-3
var_c1<-2
var_c2<-0
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

for(i in seq(from=1, to=50, by=.001)){
  temp<-MLE(i)
  #print(temp)
  if (temp>max) {
    max<-temp
    maxVar<-i
  }
}

#sigma_g
#sigma_g <- mean(s1_sig$s2_dist/s1_sig$s1_dist)*maxVar/sqrt(N_1*N_2)
sigma_g_sq <- mean(s1_sig$s2_dist)/mean(s1_sig$s1_dist)*maxVar/sqrt(N_1*N_2)


#first method to estimate c1
c1_est<-(maxVar-1-sigma_g_sq*(N_1))/(N_1)
#estimating c2
#this is not accuraute and we must fine another way to do this. 
var_1<-((s1_sig$s1_dist)*(sqrt(N_2)))-(s1_sig$s2_dist*(sqrt(N_1)))
var_1<-var(var_1)/(N_1+N_2)

expected<-(N_2*(1+N_1*var_c1)+N_1*(1+N_2*var_c2))/(N_1+N_2)

#second method to estimate c1
expected_mean_ratio<-function(c1){
  sqrt(N_1*N_2)*sigma_g_sq/(1+N_1*sigma_g_sq+N_1*c1)
}
estimate_c1 <- 0
min_rms <- 10000
min_c1 <- 0
for (i in 1:10000){
  ratio<-expected_mean_ratio(estimate_c1)
  expected_s2 <- s1_sig$s1_dist*ratio
  cur_rms <- sqrt(sum((expected_s2-s1_sig$s2_dist)^2))
  if (cur_rms < min_rms){
    min_rms<- cur_rms
    min_c1 <- estimate_c1
  }
  estimate_c1 <- estimate_c1+0.001
}