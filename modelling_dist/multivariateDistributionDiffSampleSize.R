#plotting s1 VS s2 when s1>4
#dashed red line represents all the points that replicated out of the ones that should've replicated
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#pulling lamda from normal distrubtion 
tempSd<-sqrt(2)
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=tempSd)
sampleSizeS1=1000
sampleSizeS2=100

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,2] <- rnorm(n=1,mean=lamda[i]*sqrt(sampleSizeS1),sd=1*sqrt(sampleSizeS1))
  results[i,3]<-rnorm(n=1,mean=lamda[i]*sqrt(sampleSizeS2),sd=1*sqrt(sampleSizeS2))
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

#modelling distrbutions

#covariance between s1 and s2 
#now we will plot the multivaribale disturbtion with s1 and s2 assuming known variance and covariances
#covariance matrix 
#[vs1, cs2s2]
#[cs2s2, vs2]
cov_matrix=matrix(data=NA, nrow=2, ncol=2)
cov_matrix[1,1]=3*sampleSizeS1
cov_matrix[1,2]=2*sqrt(sampleSizeS1*sampleSizeS2)
cov_matrix[2,1]=2*sqrt(sampleSizeS1*sampleSizeS2)
cov_matrix[2,2]=3*sampleSizeS2

mean_matrix=matrix(data=NA, nrow=2, ncol=1)
mean_matrix[1,1]=0
mean_matrix[2,1]=0

s1_s2_distribution<-mvrnorm(n=10000, mu=mean_matrix, Sigma = cov_matrix)
dis_frame=as.data.frame(s1_s2_distribution)
colnames(dis_frame)=c("s1", "s2")
mvnrom_plot_ss<-ggplot(data=dis_frame, mapping = aes(x=s1, y=s2))+geom_point()
#ggsave(filename="s1_s2_mvnrom.jpg")

#plotting s1 vs s2 by sampling and not assuming a distrubtion 
s1VSs2<-ggplot(data = filter(results.data), mapping = aes(x = s1, y = s2)) +
  geom_point()

#ggsave(filename="s1_s2_sampling.jpg")

#now we will plot the multivaribale disturbtion with lamda and s1
cov_matrix2=matrix(data=NA, nrow=2, ncol=2)
cov_matrix2[1,1]=2
cov_matrix2[1,2]=2
cov_matrix2[2,1]=2
cov_matrix2[2,2]=3

mean_matrix2=matrix(data=NA, nrow=2, ncol=1)
mean_matrix2[1,1]=0
mean_matrix2[2,1]=0

s1_lamda_distribution<-mvrnorm(n=10000, mu=mean_matrix2, Sigma = cov_matrix2)
dis_frame2=as.data.frame(s1_lamda_distribution)
colnames(dis_frame2)=c("lamda", "s1")

mvnrom_plot_ss<-ggplot(data=dis_frame2, mapping = aes(x=lamda, y=s1))+geom_point()+
  scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))
ggsave(filename="s1_lamda_mvnrom.jpg")


#predictions 
#bivirate probabilities / condtional 
var_g=2
sd_g_lamda=sqrt(var_g)
sd_g_s=sqrt(1+var_g)
s1=5.2
cor_lam_S=(var_g)/(sd_g_lamda*sd_g_s)

#correction for winners curse
lamda_pred<-rnorm(n=1000, mean=(var_g*s1)/(1+var_g), sd=sqrt(var_g/(1+var_g)))
#tempMean=(sd_g_lamda/sd_g_s)*(cor_lam_S*s1)
#real mean? 
#mean(subset(results.data, s1>5.2)$lamda)
#mean(results.data$lamda[results.data$s1>5.2])

s2_pred<-rnorm(n=1, mean=(var_g*s1)/(1+var_g), sd=sqrt((1+var_g)^2-(var_g)^2)/(1+var_g))


