#reading in libraries
# library(tidyverse)
# library(MASS)
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(reshape2)

#reading in data 
filename<-"./files/4_19820697_data_upbuilt_filtered_upbuilt.csv" #average #5 vs 12.96
data<-read.csv(filename, sep=",")

#creating test statistic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)


#variables
M=nrow(data)
var_given=data$trait.var[1]
threshold=data$p.thresh[1]
z_score_nom <- qnorm(0.05,lower.tail = FALSE)
z_score <- qnorm(0.05/M,lower.tail =FALSE)
Z_Score_s1<-qnorm(threshold,lower.tail =FALSE)
N_1<-mean(data$n.disc)
N_2<-mean(data$n.rep)
#estimating sigma using s1 
sigma_g_est <-estimate_sigG()[1]
maxVar<-estimate_sigG()[2]
c1_est<-estimate_c1(maxVar, sigma_g_est)
c2_est<-.002


#functions 
MLE<-function(var){
  M_Org<-0.05/threshold
  estimate<-choose(M_Org,M)
  estimate<-1
  for(i in results.data$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))
  }
  estimate<-estimate*(pnorm(Z_Score_s1, mean=0, sd=sqrt(var)))^(M_Org-M)
}

estimate_sigG<-function(){
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
  sigma_g_est <- mean(results.data$s2)/mean(results.data$s1)*maxVar/sqrt(N_1*N_2)
  return(c(sigma_g_est, maxVar))
}

estimate_c1<-function(maxVar,sig_g){
  c1_est<-(maxVar-1-sig_g*(N_1))/(N_1)
  return(c1_est)
}


calculate_upperCI<-function(sigma,s1,sampleS1, sampleS2, c1, c2){
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*sigma^4)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(sigma,s1,sampleS1, sampleS2, c1, c2){
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-sd_S2^2-((sampleS1*sampleS2*sigma^4)/(sd_S1^2))
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}

calculate_mean<-function(sigma,s1,sampleS1,sampleS2, c1, c2){
  sd_S1<-sqrt(sampleS1*sigma^2+1+c1*sampleS1)
  sd_S2<-sqrt(sampleS2*sigma^2+1+c2*sampleS2)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
}


#plotting 

pred_obs<-ggplot(data = results.data, mapping = aes(x = s1, y = s2)) +
  geom_point()+
  stat_function(fun=calculate_lowerCI,args=list(sigma=sigma_g_est, sampleS1<-N_1, sampleS2<-N_2, c1=abs(c1_est), c2=c2_est), color="paleturquoise2")+
  stat_function(fun=calculate_upperCI,args=list(sigma=sigma_g_est, sampleS1<-N_1, sampleS2<-N_2, c1=abs(c1_est), c2=c2_est), color="paleturquoise2")+
  stat_function(fun=calculate_mean, args=list(sigma=sigma_g_est, sampleS1<-N_1, sampleS2<-N_2, c1=abs(c1_est), c2=c2_est), color="palevioletred1")
pred_obs
