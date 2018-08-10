#testing on datasets from article 


#reading in libraries
library(mvtnorm)
library(tidyverse)
# library(MASS)
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(reshape2)

#read in file 
#filename<-"./files/1_25282103_data_upbuilt_filtered_upbuilt.csv"
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
for(i in seq(from=0.1, to=1000, by=.1)){
  temp<-MLE(i)
  #print(temp)
  if (temp>max) {
    max<-temp
    maxVar<-i
  }
}

#first method to estimate var_g
expected_mean_ratio<-function(sigma_g_2){
  sigma_g_2/maxVar
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
  sigma_g_estimator <- sigma_g_estimator+0.01
}

#var_g_1 <- mean(results.data$s2/results.data$s1)*maxVar/sqrt(sampleSizeS1*sampleSizeS2)
#estimating c2
c1_est2<-maxVar-1/N_1-var_g_est2


#estimating sigmac2
MLE_joint_probability<-function(var_g, var_c1, var_c2){
  cov_matrix=matrix(data=NA, nrow=2, ncol=2)
  cov_matrix[1,1]=var_g+var_c1+1/N_1
  cov_matrix[1,2]=var_g
  cov_matrix[2,1]=var_g
  cov_matrix[2,2]=var_g+var_c2+1/N_2
  
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
for(i in seq(from=0,to=50, by=0.0002)){
  temp<-MLE_joint_probability(var_g_est2, c1_est2, i)
  if(temp>max){
    max<-temp
    c2_est<-i
  }
}

#checking distribution

calculate_upperCI<-function(s1,sampleS1, sampleS2, var_g, c1, c2){
  sd_S1<-sqrt(var_g+1/sampleS1+c1)
  sd_S2<-sqrt(var_g+1/sampleS2+c2)
  mean<-(var_g*s1)/(sd_S1^2)
  var2<-sd_S2^2-(var_g^2)/(sd_S1^2)
  error <- qnorm(0.975)*sqrt(var2)
  mean+error
}

calculate_lowerCI<-function(s1,sampleS1, sampleS2, var_g, c1, c2){
  sd_S1<-sqrt(var_g+1/sampleS1+c1)
  sd_S2<-sqrt(var_g+1/sampleS2+c2)
  mean<-(var_g*s1)/(sd_S1^2)
  var2<-sd_S2^2-(var_g^2)/(sd_S1^2)
  error <- qnorm(0.975)*sqrt(var2)
  return(mean-error)
}

calculate_mean<-function(s1,sampleS1,sampleS2, var_g, c1, c2){
  sd_S1<-sqrt(var_g+1/sampleS1+c1)
  sd_S2<-sqrt(var_g+1/sampleS2+c2)
  mean<-(var_g*s1)/(sd_S1^2)
}


# checkingDist<-ggplot(data = filter(results.data), mapping = aes(x = s1, y = s2)) +
#   geom_point()+
#   #scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+
#   stat_function(fun=calculate_lowerCI,args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2, var_g=var_g_est2, c1=c1_est2, c2=c2_est),linetype = 2, color="red")+
#   stat_function(fun=calculate_upperCI,args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2, var_g=var_g_est2, c1=c1_est2, c2=c2_est),linetype = 2, mapping =aes(color="Confidence Interval"))+
#   stat_function(fun=calculate_mean, args=list(sampleS1=sampleSizeS1, sampleS2=sampleSizeS2, var_g=var_g_est2, c1=c1_est2, c2=c2_est), mapping=aes(color="Mean"))
# 
# checkingDist

#Predicting replication rate 
ZScore_2<-qnorm(0.05/nrow(results.data),lower.tail =FALSE)

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

calculate_pcondtional_mean<-function(s1, var_g, var_c1, var_c2){
  mean<-(var_g*s1)/(1/N_1+var_g+var_c1)
  var<-var_g+var_c2+1/N_2-((var_g^2)/(var_g+var_c1+1/N_1))
  return (mean)
}

calculate_pcondtional_var<-function(s1, var_g, var_c1, var_c2){
  mean<-(var_g*s1)/(1/N_1+var_g+var_c1)
  var<-var_g+var_c2+1/N_2-((var_g^2)/(var_g+var_c1+1/N_1))
  return (var)
}

theo_rep <- sum(calculate_pcondtional(results.data$s1, var_g_est2, c1_est2, c2_est))/nrow(results.data)

s1_s2_sig<-results.data %>% filter(s2>ZScore_2 | s2<(-ZScore_2))
repRate=nrow(s1_s2_sig)/nrow(results.data)
try.temp <- cbind(results.data$s1,results.data$s2,
                  calculate_pcondtional(results.data$s1, var_g_est2, c1_est2, c2_est),
                  calculate_pcondtional_mean(results.data$s1, var_g_est2, c1_est2, c2_est),
                  calculate_pcondtional_var(results.data$s1, var_g_est2, c1_est2, c2_est))
colnames(try.temp)<-c("s1","s2","prob","pred_mean","pred_var")

prd_rep_cnt <- sum(calculate_pcondtional(results.data$s1, var_g_est2, c1_est2, c2_est))
prd_rep_cnt_f <- formatC(prd_rep_cnt, width = 4, format="fg")
var_g_f <- formatC(var_g_est2, width = 4, format="fg")
var_c1_f <- formatC(c1_est2, width = 4, format="fg")
var_c2_f <- formatC(c2_est, width = 4, format="fg")

results <- paste(as.character(prd_rep_cnt_f),as.character(var_g_f),
                 as.character(var_c1_f),as.character(var_c2_f),sep=" ")
results <- paste(results,"\n")
cat(results)

