#testing on datasets from article 


#reading in libraries
# library(tidyverse)
# library(MASS)
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(reshape2)


#read in file 
#filename<-"./files/1_19820697_data_upbuilt_filtered_upbuilt.csv" #straight+too few data
#filename<-"./files/2_19820697_data_upbuilt_filtered_upbuilt.csv" #straight+too few data
#filename<-"./files/3_19820697_data_upbuilt_filtered_upbuilt.csv" #not enough sig s2
#filename<-"./files/4_19820697_data_upbuilt_filtered_upbuilt.csv" #average #5 vs 12.96
#filename<-"./files/5_19820697_data_upbuilt_filtered_upbuilt.csv" #all sig #3 vs 2.997
#filename<-"./files/1_19343178_data_upbuilt_filtered_upbuilt.csv" #good #6 vs 21.16
#filename<-"./files/1_19557161_data_upbuilt_filtered_upbuilt.csv" #average #6 vs 24.31 
#filename<-"./files/1_20010834_data_upbuilt_filtered_upbuilt.csv" #okay #6 vs 29.06
#filename<-"./files/1_20887962_data_upbuilt_filtered_upbuilt.csv" #straight
#filename<-"./files/1_21738478_data_upbuilt_filtered_upbuilt.csv" #good #13 vs 31.5
#filename<-"./files/1_21909110_data_upbuilt_filtered_upbuilt.csv" #good but large variance #11 vs 23.78
#filename<-"./files/2_21909110_data_upbuilt_filtered_upbuilt.csv" #good #23 vs 30.82
#filename<-"./files/1_24952745_data_upbuilt_filtered_upbuilt.csv" #accetable 

#filename<-"25282103_data_upbuilt_filtered_upbuilt.csv" #good 
#filename<-"21909110_data_upbuilt_filtered_upbuilt.csv" #good ratio and not is the same and its ok
#filename<-"21947420_data_upbuilt_filtered_upbuilt.csv" #good will be off but shoudl be good? 
#filename<-"21998595_data_upbuilt_filtered_upbuilt.csv" #good ratio is good 
#filename<-"22021425_data_upbuilt_filtered_upbuilt.csv" #good ratio is good
#filename<-"22267201_data_upbuilt_filtered_upbuilt.csv" #good #very good 
#filename<-"22504420_data_upbuilt_filtered_upbuilt.csv" #good
#filename<-"23118974_data_upbuilt_filtered_upbuilt.csv" #mostly not sigfnicant
#filename<-"23263486_data_upbuilt_filtered_upbuilt.csv" #kinda good
#filename<-"23669352_data_upbuilt_filtered_upbuilt.csv" #idk
#filename<-"23658558_data_upbuilt_filtered_upbuilt.csv" #hm
#filename<-"24430505_data_upbuilt_filtered_upbuilt.csv" #hm idk

#filename<-"25035420_data_upbuilt_filtered_upbuilt.csv" #straight
#filename<-"22267201_data_upbuilt_filtered_upbuilt.csv"

#filename<-"./files/1_19820697_data_upbuilt_filtered_upbuilt.csv"

args = commandArgs(trailingOnly=TRUE)
filename<-args[1]

data<-read.csv(filename, sep=",")

#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)

#checking distribution
# checkingDist<-ggplot(data = filter(results.data), mapping = aes(x = s1, y = s2)) +
#   geom_point()+
#   scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))
# 
# checkingDist


#needed functions and math
M=nrow(data)
var=data$trait.var[1]
#sampleSize
sigma=sqrt(var)
threshold=data$p.thresh[1]
z_score_nom <- qnorm(0.05,lower.tail = FALSE)
z_score <- qnorm(0.05/M,lower.tail =FALSE)

# sampleSizeS1=data$n.disc
# sampleSizeS2=data$n.rep
sampleSizeS1<-mean(data$n.disc)
sampleSizeS2<-mean(data$n.rep)
results.data$actual_rep = rep(0,M)

for (i in 1:M){
  # if (results.data$s2[i]>z_score | results.data$s2[i]<(-z_score))
  #   results.data$actual_rep[i] = 1
  if (results.data$s2[i]>z_score_nom && results.data$s1[i]>0)
    results.data$actual_rep[i] = 1
  if (results.data$s1[i]< 0 && results.data$s2[i]<(-z_score_nom))
    results.data$actual_rep[i] = 1
}
#calculating condtional with different sample sizes.
#this one is assuming same sample size
# calculate_pcondtional<-function(s1){
#   mean<-(sigma^2*s1)/(1+sigma^2)
#   var2=1+((sigma^2)/(1+sigma^2))
#   p<- (1-pnorm(zScore, mean, sqrt(var2)))+pnorm(-zScore, mean, sqrt(var2))
#   return(p)
# }

#probability of s2 replicating given s1
calculate_pcondtional<-function(s1,sampleS1, sampleS2){
  sd_S1<-sqrt(sampleS1*sigma^2+1)
  sd_S2<-sqrt(sampleS2*sigma^2+1)
  mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)
  var2<-1+((sampleS2*sigma^2)/(sd_S1)^2)
  p <- s1
  for (i in 1:NROW(s1)){
    if (s1[i]>0){
     #p[i]<-(1-mean(pnorm(z_score, mean[i], sqrt(var2))))
      p[i]<-(1-pnorm(z_score, mean[i], sqrt(var2)))
      
    }
    else{
     # p[i]<-mean(pnorm(-z_score, mean[i], sqrt(var2)))
      p[i]<-pnorm(-z_score, mean[i], sqrt(var2))
    }
  }
  return(p)
}

#predicing repication using the ratio 
# calculate_pcondtional_ratio<-function(s1,sampleS1, sampleS2,ratio){
#   sd_S1<-sqrt(sampleS1*sigma^2+1)
#   sd_S2<-sqrt(sampleS2*sigma^2+1)
#   mean<-(sqrt(sampleS1)*sqrt(sampleS2)*sigma^2*s1)/(sd_S1^2)*(ratio)
#   var2<-(1+((sampleS2*sigma^2)/(sd_S1)^2))
#   p <- s1
#   for (i in 1:NROW(s1)){
#     if (s1[i]>0){
#       #p[i]<-(1-mean(pnorm(z_score, mean[i], sqrt(var2))))
#       p[i]<-(1-pnorm(z_score, mean[i], sqrt(var2)))
#       
#     }
#     else{
#       # p[i]<-mean(pnorm(-z_score, mean[i], sqrt(var2)))
#       p[i]<-pnorm(-z_score, mean[i], sqrt(var2))
#     }
#   }
#   return(p)
# }

#observed replication rate our way
# s1_sig<-nrow(filter(results.data, s1>zScore|s1<(-zScore)))
# s2_sig_givens1<-nrow(filter(filter(results.data, s1>zScore), s2>zScore)) + nrow(filter(filter(results.data, s1<(-zScore)), s2<(-zScore)))
# repRate=s2_sig_givens1/nrow(results.data)

#observed rep count
obs_rep_cnt <- sum(results.data$actual_rep)


#theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector, sampleSizeS1, sampleSizeS2))/N
#theo_rep2 <- sum(calculate_pcondtional(s1_sig_vector,sampleSizeS1, sampleSizeS2))/N
results.data$pred_prob = calculate_pcondtional(results.data$s1,sampleSizeS1, sampleSizeS2)
prd_rep_cnt <- sum(calculate_pcondtional(results.data$s1,sampleSizeS1, sampleSizeS2))
m_f <- formatC(M, width = 4, format="d")
obs_rep_cnt_f <- formatC(obs_rep_cnt, width = 4, format="d")
prd_rep_cnt_f <- formatC(prd_rep_cnt, width = 4, format="fg")
results <- paste(as.character(m_f),as.character(obs_rep_cnt_f),as.character(prd_rep_cnt_f)," ",sep=" ")
cat(results)

#using adjusted mean for ratio
# ratio <- ((results.data$s1)*(sqrt(sampleSizeS2)))/((results.data$s2)*(sqrt(sampleSizeS1)))
# mean(ratio)
# results.data$pred_prob_adj <- calculate_pcondtional_ratio(results.data$s1,sampleSizeS1, sampleSizeS2,mean(ratio))
# adj_cnt <- sum(results.data$pred_prob_adj)
# temp2<-((tempData$s1_dist)*(sqrt(N_2)))-((tempData$s2_dist1)*(sqrt(N_1)))*mean(ratio)
# temp2<-temp2/(sqrt(N_2+N_1))
# mean(temp2)
# var(temp2)
