#confounding estimate by splitting the data set 
library(caTools)
library(tidyverse)
library(dplyr)


var_g<-2*.75
var_c1<-2*.25
M<-1000
sampleSizeS1<-100
sampleSizeS2<-50
ZScore<-qnorm(0.05/M,lower.tail = FALSE)

lambda<-rnorm(n=M, mean=0, sd=sqrt(var_g))
s1<-as.vector(matrix(nrow=M, ncol=1))
delta1<-rnorm(n=M, mean=0, sd=sqrt(var_c1))

for (i in 1:M){
  s1[i]<-rnorm(n=1,mean=sqrt(N_1)*(lambda[i]+delta1[i]),sd=1)
}

results.data<-data.frame(lambda,s1)

split = sample.split(results.data$lambda, SplitRatio = .5)
set1 = subset(results.data, split == TRUE) #discovery study 
set2 = subset(results.data, split == FALSE) #replication study 



N_1<-sampleSizeS1
N_2<-sampleSizeS2
num_s1<-sum(set1$s1>ZScore|set1$s1<(-ZScore))
s1_sig<-set1 %>% filter(s1>ZScore | s1<(-ZScore))


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
var_g_est1 <- mean(s1_sig$s2/s1_sig$s1)*maxVar/sqrt(sampleSizeS1*sampleSizeS2)

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
  stat_function(fun=calculate_lowerCI,args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2,var_g=var_g_est2, c1=c1_est2, c2=c2_est),linetype = 2, color="red")+
  stat_function(fun=calculate_upperCI,args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2,var_g=var_g_est2, c1=c1_est2, c2=c2_est),linetype = 2, mapping =aes(color="Confidence Interval"))+
  stat_function(fun=calculate_mean, args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2,var_g=var_g_est2, c1=c1_est2, c2=c2_est), mapping=aes(color="Mean"))+
  xlab("Discovery Sample Statistics")+ylab("Replication Sample Statistics")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("Confidence Interval"="red", "Test Statistics"="dodgerblue3", "Mean"="black"))
pred_obs

ggsave(filename ="sim_confInterval_confoudingEST2.jpg")



#calculating s2 given s1
ZScore_2<-qnorm(0.05/nrow(s1_sig),lower.tail =FALSE)
calculate_pcondtional<-function(s1, var_g, var_c1, var_c2){
  mean<-(sqrt(N_1*N_2)*var_g*s1)/(1+N_1*var_g+N_1*var_c1)
  #var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  var<-(N_2*var_g)+(N_2*var_c2)+1-((N_1*N_2*var_g^2)/(N_1*var_g+N_1*var_c1+1))
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
      p[i]<-(1-pnorm(ZScore_2*sqrt(N_2), mean[i], sqrt(var)))
    }
    else{
      p[i]<-pnorm(-ZScore_2*sqrt(N_2), mean[i], sqrt(var))
    }
  }
  return (p)
}

calculate_pcondtional_mean<-function(s1, var_g, var_c1, var_c2){
  mean<-(sqrt(N_1*N_2)*var_g*s1)/(1+N_1*var_g+N_1*var_c1)
  #var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  var<-(N_2*var_g)+(N_2*var_c2)+1-((N_1*N_2*var_g^2)/(N_1*var_g+N_1*var_c1+1))
  return (mean)
}

calculate_pcondtional_var<-function(s1, var_g, var_c1, var_c2){
  mean<-(sqrt(N_1*N_2)*var_g*s1)/(1+N_1*var_g+N_1*var_c1)
  #var<-(1-(sigma_g^4)/((1+sigma_g^2+sigma_c1^2)*(1+sigma_g^2+sigma_c2^2)))*(1+sigma_g^2+sigma_c2^2)
  var<-(N_2*var_g)+(N_2*var_c2)+1-((N_1*N_2*var_g^2)/(N_1*var_g+N_1*var_c1+1))
  return (var)
}

#rep rate 
#s1_sig<-nrow(filter(data, s1>5.2|s1<(-5.2)))
s1_s2_sig<-s1_sig %>% filter(s2_2>ZScore_2 | s2_2<(-ZScore_2))
repRate=nrow(s1_s2_sig)/nrow(s1_sig)

#theoretical reprate
theo_rep <- sum(calculate_pcondtional(s1_sig$s1, var_g_est2, c1_est2, c2_est))/nrow(s1_sig)