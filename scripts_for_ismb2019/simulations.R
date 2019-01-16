rm(list=ls())
library(tidyverse)

sigma2g=0.2 #variance in trait due to genetic effects
sigma2con1=0.1  #confounding in study 1
sigma2con2=0.05 #confounding in study 2
sigma2e=1 #noise
N1=1000 #sample size study 1
N2=100 #sample size study 2
#creates dataframe to collect simulations
numtests=1000
temp=matrix(NA,numtests,1)
df=data.frame(testnum=c(1:numtests) ,s1=temp,s2=temp,N1=temp,N2=temp,lamda=temp, delta1=temp,delta2=temp,noise1=temp, noise2=temp)

for (testnum in df$testnum){
  lamda=rnorm(n=1,mean = 0,sd = sqrt(sigma2g))
  delta1=rnorm(n=1,mean = 0,sd = sqrt(sigma2con1))
  delta2=rnorm(n=1,mean = 0,sd = sqrt(sigma2con2))
  noise1=rnorm(n=1,mean = 0,sd = sqrt(sigma2e))
  noise2=rnorm(n=1,mean = 0,sd = sqrt(sigma2e))
  
  s1 = sqrt(N1)*(lamda+delta1) + noise1
  s2 = sqrt(N2)*(lamda+delta2) + noise2
  
  df[testnum,]=c(testnum, s1, s2, N1, N2, lamda, delta1, delta2, noise1, noise2)
  
}

sd <- sqrt(N2*sigma2g + N2*sigma2con2 + 1 - (N1*N2*sigma2g*sigma2g)/(N1*sigma2g + N1*sigma2con1 +1)) #scalar
slope <-  (sqrt(N1*N2) *sigma2g) / (N1*sigma2g + N1*sigma2con1 +1)

slope_noConfounding <- sqrt((sqrt(N1*N2) *sigma2g)/(N1*sigma2g + 1))
sd_noConfounding <- 1 + (N2*sigma2g)/(N1*sigma2g +1)


ggplot(df)+
  geom_point(aes(x=s1, y=s2, color="s"), color="grey")+
  geom_abline(aes(color="Winner's curse"), intercept = 0, slope = slope_noConfounding, color="#0571b0", size=1.25)+
  geom_abline(intercept = -2*sd_noConfounding, slope = slope_noConfounding, linetype=2, color="#0571b0", size=1.25)+
  geom_abline(intercept = 2*sd_noConfounding, slope = slope_noConfounding, linetype=2, color="#0571b0", size=1.25)+
  geom_abline(aes(color="Winner's curse + confounding"), intercept = 0, color="#ca0020", slope = slope, size=1.25)+
  geom_abline(intercept = -2*sd, slope = slope, color="#ca0020", linetype=2, size=1.25)+
  geom_abline(intercept = 2*sd, slope = slope, color="#ca0020", linetype=2, size=1.25)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name='My Lines', values=c("ca0020"))+
  ylab("Replication Summary Statistics")+
  xlab("Discovery Summary Statistics")+
  theme(legend.position = 'top')+
  scale_color_manual(values = c("s"="grey", "Winner's curse + confounding" = "#ca0020", "Winner's curse"="#0571b0"))

cov(df$s1,df$s2)
cov(df$s1,df$s2)/(sqrt(N1)*sqrt(N2))


lambda1 = rnorm(n=100000, mean=0,sd = sqrt(sigma2g) )
lambda2 = rnorm(n=100000, mean=0,sd = sqrt(sigma2g) )
cov(lambda1, lambda2)