#libraries
#install.packages("mvtnorm")
library(mvtnorm)
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)


#simulating data 
var_g<-4*.75
var_c1<-4*.25
var_c2<-4*.25
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

data1$s1_2<-(data1$s1)/sqrt(sampleSizeS1)
data1$s2_2<-(data1$s2)/sqrt(sampleSizeS2)
s1_sig<-data1 %>% filter(s1_2>ZScore | s1_2<(-ZScore))
data1<-data1 %>% filter(s1_2>ZScore | s1_2<(-ZScore))

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
  geom_point(mapping =aes(color="Test Statistics"))+
  stat_function(fun=calculate_lowerCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=var_c1, c2=var_c2),linetype = 2, color="red")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=var_c1, c2=var_c2),linetype = 2, mapping =aes(color="Confidence Interval"))+
  stat_function(fun=calculate_mean, args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=var_c1, c2=var_c2), mapping=aes(color="Mean"))+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Confidence Interval"="red", "Test Statistics"="dodgerblue3", "Mean"="black"))
pred_obs

ggsave(filename ="sim_confInterval_confouding2.jpg")


pred_obs<-ggplot(data = data1, mapping = aes(x = s1, y = s2)) +
  geom_point(mapping =aes(color="Test Statistics"))+
  stat_function(fun=calculate_lowerCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=0, c2=0),linetype = 2, color="red")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=0, c2=0),linetype = 2, mapping =aes(color="Confidence Interval"))+
  stat_function(fun=calculate_mean, args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=0, c2=0), mapping=aes(color="Mean"))+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Confidence Interval"="red", "Test Statistics"="dodgerblue3", "Mean"="black"))
pred_obs

ggsave(filename ="sim_confInterval_withoutconfouding2.jpg")



#estimating confouding and heriability from data 
#s1 variance
N_1<-sampleSizeS1
N_2<-sampleSizeS2
num_s1<-sum(data1$s1_2>ZScore|data1$s1_2<(-ZScore))


MLE<-function(var){
  estimate<-choose(M,num_s1)
  for(i in s1_sig$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))
  }
  estimate<-estimate*(pnorm(ZScore*sqrt(sampleSizeS1), mean=0, sd=sqrt(var))-pnorm((-ZScore)*sqrt(sampleSizeS1), mean=0, sd=sqrt(var)))^(M-num_s1)
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
  expected_s2 <- s1_sig$s1*ratio
  cur_rms <- sqrt(sum((expected_s2-s1_sig$s2)^2))
  if (cur_rms < min_rms){
    min_rms<- cur_rms
    var_g_est2 <- sigma_g_estimator
  }
  sigma_g_estimator <- sigma_g_estimator+0.01
}

#another var_g estimator
var_g_1 <- mean(s1_sig$s2/s1_sig$s1)*maxVar/sqrt(sampleSizeS1*sampleSizeS2)

#est c1
c1_est2<-(maxVar-1-var_g_1*(N_1))/(N_1)

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
  
  estimate<-choose(M,num_s1)
  
  vector<-(s1_sig[,c(4,5)])
  prob<-dmvnorm(x=vector, mean = mean_matrix, sigma = cov_matrix, log = FALSE)
  
  for(i in prob){
    estimate<-estimate*i
  }
  return(estimate)
}

max<-0
c2_est<-0
for(i in seq(from=0,to=10, by=0.001)){
  temp<-MLE_joint_probability(var_g_1, c1_est2, i)
  if(temp>max){
    max<-temp
    c2_est<-i
  }
}

pred_obs<-ggplot(data = data1, mapping = aes(x = s1, y = s2)) +
  geom_point(mapping =aes(color="Test Statistics"))+
  stat_function(fun=calculate_lowerCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=c1_est2, c2=c2_est),linetype = 2, color="red")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=c1_est2, c2=c2_est),linetype = 2, mapping =aes(color="Confidence Interval"))+
  stat_function(fun=calculate_mean, args=list(sampleS1<-sampleSizeS1, sampleS2<-sampleSizeS2, c1=c1_est2, c2=c2_est), mapping=aes(color="Mean"))+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Confidence Interval"="red", "Test Statistics"="dodgerblue3", "Mean"="black"))
pred_obs

ggsave(filename ="sim_confInterval_confoudingEST2.jpg")


#calculating s2 given s1
#calculating s2 given s1
calculate_pcondtional<-function(s1, var_g, var_c1, var_c2){
  mean<-(sqrt(N_1*N_2)*var_g*s1)/(1+N_1*var_g+N_1*var_c1)
  #var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  var<-(N_2*var_g)+(N_2*var_c2)+1-((N_1*N_2*var_g^2)/(N_1*var_g+N_1*var_c1+1))
  p<- (1-pnorm(5.2, mean, sqrt(var)))+pnorm(-5.2, mean, sqrt(var))
  return(p)
}


#rep rate 
#s1_sig<-nrow(filter(data, s1>5.2|s1<(-5.2)))
ZScore_2<-qnorm(0.05,lower.tail =FALSE)
s1_s2_sig<-s1_sig %>% filter(s2_2>ZScore_2 | s2_2<(-ZScore_2))
repRate=nrow(s1_s2_sig)/M

#theoretical reprate
theo_rep <- sum(calculate_pcondtional(s1_sig$s1, var_g_1, c1_est2, c2_est))/M

#rep rate = 0.0506
#tho=0.05
#c1 est 1.219
#c2 est 1.078
#trait var est 2.89



