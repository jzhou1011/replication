library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#pulling lambda from normal distrubtion 
sigma<-sqrt(2)
N=10000
sampleSize=10000
lambda <- rnorm(n=N, mean=0, sd=sigma)

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lambda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lambda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lambda", "s1", "s2")

true_lambda <- function(lambda){
  return(lambda)
}

#the predicted lambda is the lambda that we predict using the multivariate distribution of lambda and s1 (given s1)
compute_mean <- function(s1){
  sigma^2*s1/(1+sigma^2)
}

compute_mean_square <- function(s1){
  sqrt(sigma^2/(1+sigma^2))*s1
}

results.data$pred_lambda <- compute_mean(results.data$s1)
results.data$pred_lambda_sqrt <- compute_mean_square(results.data$s1)


s1_val <- results.data[c("lambda","s1")]
s1_val$grouping = rep("s1 as the predicted Effect Size",10000)
colnames(s1_val) = c("lambda","value")
pred_val <- results.data[c("lambda","pred_lambda")]
pred_val$grouping = rep("predicted effect size using model",10000)
colnames(pred_val) = c("lambda","value")
result.sp.data <- rbind(s1_val,pred_val)
#pred_lambda_sq <- results.data[c("lambda","pred_lambda_sqrt")]
#pred_lambda_sq$grouping<- rep("sqrt",10000)
#colnames(pred_lambda_sq) = c("lambda","value")
result.sp.data <- rbind(s1_val,pred_val)
colnames(result.sp.data) <- c("True_Effect_Size","Predicted_Effect_Size","grouping")

trueVSpred<-ggplot(data = result.sp.data, mapping = aes(x = True_Effect_Size, y =Predicted_Effect_Size, colour=(result.sp.data$grouping)))+
  geom_point()+scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+ 
  stat_function(fun=true_lambda,geom="line",colour="blue")+
  xlab("True Effect Size")+ylab("Predicted Effect Size")+
  theme(legend.position = "right")+
  scale_color_manual(name = element_blank(), # or name = element_blank()
                     values = c("predicted effect size using model"="dodgerblue2", "s1 as the predicted Effect Size"="firebrick"))
#trueVSpred<-ggplot(data = results.data, mapping = aes(x = lambda, y = s1_pred)) +geom_point()+scale_y_continuous(breaks=seq(-8, 8, 1), limits=c(-8,8))+scale_x_continuous(breaks=seq(-8, 8, 1), limits=c(-8, 8))+ stat_function(fun=true_lambda,geom="line",colour="blue")
trueVSpred
ggsave(filename="trueVSpred.jpg")
