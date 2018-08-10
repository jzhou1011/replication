library(tidyverse)
library(dplyr)

var_g<-2
var_c1<-2
var_c2<-4
M<-10000
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
rep_rate<-nrow(s1_s2_sig)/nrow(s1_sig)

results.data$s1_cor <- results.data$s1_dist*sqrt(1+N_1*var_g)/sqrt(1+N_1*var_g+N_1*var_c1)
results.data$s2_cor <- results.data$s2_dist*sqrt(1+N_2*var_g)/sqrt(1+N_2*var_g+N_2*var_c2)
s1_sig_cor <- filter(results.data,s1_cor>zscore1)
s1_s2_sig_cor <- filter(s1_sig_cor,s2_dist>zscore2)
rep_rate_cor<-nrow(s1_s2_sig_cor)/nrow(s1_sig_cor)