library(ggplot2)
library(dplyr)

# args = commandArgs(trailingOnly=TRUE)
# filename<-args[1]
# outfile <- args[2]
# 
# 
# data<-read.table(filename, header=T)
# stopifnot(nrow(data)>1)
#t <- data[1,5]

#simulate data
var_g<-3
var_c1<-1
var_c2<-1
M<-1000
N_1<-2
N_2<-3
lambda<-rnorm(n=M, mean=0, sd=sqrt(var_g))
s1_dist<-as.vector(matrix(nrow=M, ncol=1))
s2_dist<-as.vector(matrix(nrow=M, ncol=1))
delta1<-rnorm(n=M, mean=0, sd=sqrt(var_c1))
delta2<-rnorm(n=M, mean=0, sd=sqrt(var_c2))

for (i in 1:M){
  s1_dist[i]<-rnorm(n=1,mean=sqrt(N_1)*(lambda[i]+delta1[i]),sd=1)
  s2_dist[i]<-rnorm(n=1,mean=sqrt(N_2)*(lambda[i]+delta2[i]),sd=1)
}

data_init<-data.frame(s1_dist, s2_dist)
data_init$N_1<-rep(N_1,M)
data_init$N_2<-rep(N_2,M)
data<-filter(data_init,abs(s1_dist)>abs(qnorm(0.25/M)))
t <- 0.05/nrow(data)



#estimate variance components
estimate_sigma_g <- function(s1, s2, n1, n2){
  D = var(s1 + s2) - var(s1-s2)
  return (max((D/(4*sqrt(as.numeric(n1)*as.numeric(n2)))), 0.000000000000000000000000000001) )
}

estimate_sigma_c1 <- function(s1, n1, sigma_g){
  num <- var(s1) - as.numeric(n1)*sigma_g - 1
  #	return(num/n1)
  return(max(num/as.numeric(n1), 0.000000000000000000000000000001))
}

estimate_sigma_c2 <- function(s2, n2, sigma_g){
  num <- var(s2) - as.numeric(n2)*sigma_g -1
  #	return(num/n2)
  return(max(num/as.numeric(n2), 0.000000000000000000000000000001))
}


predict_replication <- function(mean, sd, z){
  #calculate predicted replication rate
  lower <-  pnorm(z, mean, sd)
  upper <- 1- pnorm(-z, mean, sd)
  return(lower + upper)
}


plot_conditional <- function(data, slope, sd, slope_noConfounding, sd_noConfounding){
  maxS <- max(c(abs(data[,1]), abs(data[,2]), 9))
  ggplot(data)+
    geom_point(aes(x=data[,1], y=data[,2], color="s"), color="grey")+
    geom_abline(aes(color="Winner's curse"), intercept = 0, slope = slope_noConfounding, color="#0571b0", size=1.25)+
    geom_abline(intercept = -2*sd_noConfounding, slope = slope_noConfounding, linetype=2, color="#0571b0", size=1.25)+
    geom_abline(intercept = 2*sd_noConfounding, slope = slope_noConfounding, linetype=2, color="#0571b0", size=1.25)+
    geom_abline(aes(color="Winner's curse + confounding"), intercept = 0, color="#ca0020", slope = slope, size=1.25)+
    geom_abline(intercept = -2*sd, slope = slope, color="#ca0020", linetype=6, size=1.25)+
    geom_abline(intercept = 2*sd, slope = slope, color="#ca0020", linetype=6, size=1.25)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    #		scale_fill_manual(name='My Lines', values=c("ca0020"))+
    ylab("Replication Summary Statistics")+
    xlab("Discovery Summary Statistics")+
    theme(legend.position = 'top')+
    scale_color_manual(values = c("s"="grey", "Winner's curse + confounding" = "#ca0020", "Winner's curse"="#0571b0"))+
    coord_cartesian(xlim = c(-maxS, maxS), ylim = c(-maxS, maxS)) 
  
  ggsave(paste0(args[2], ".png"))
}

plotWinnersCurse <- function(data){
  z <- qnorm(t/2)
  maxS <- max(c(abs(data[,1]), abs(data[,2]), 9))
  ggplot(data)+
    geom_point(aes(x=data[,1], y=data[,2], color="s"), color="grey")+
    geom_vline(xintercept=z, size=1.25, linetype=2)+
    geom_vline(xintercept=-z, size=1.25, linetype=2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab("Replication Summary Statistics")+
    xlab("Discovery Summary Statistics")+
    coord_cartesian(xlim = c(-maxS, maxS), ylim = c(-maxS, maxS))
  ggsave(paste0(args[2], "_wc.png"))
}

#calculate predicted replication rate under model with no confounding
predict_no_confounding <- function(data, z){
  #currently assume sample size is same for all variants
  n1 <- as.numeric(data[1,3])
  n2 <- as.numeric(data[1,4])
  sigma_g <- estimate_sigma_g(data[,1], data[,2], n1, n2)
  
  #compute mean and standard deviation of conditional distribution	
  mean <- data[,1] * (sqrt(n1*n2) *sigma_g)/(n1*sigma_g + 1) #vector
  sd <- sqrt(1 + (n2*sigma_g)/(n1*sigma_g +1))	#scalar
  slope <- (sqrt(n1*n2) *sigma_g)/(n1*sigma_g + 1)
  
  #calculate replication rate across significant SNPs in initial study
  predicted_replication <- 0
  for(i in 1:nrow(data)){
    predicted_replication <- predicted_replication +  predict_replication(mean[i], sd, z) 
    
  }
  
  return(c(predicted_replication/nrow(data), sigma_g, slope, sd))
}

#calculate predicted replication rate under model with confounding
predict_with_confounding <- function(data, z){
  n1 <- as.numeric(data[1,3])
  n2 <- as.numeric(data[1,4])
  sigma_g <- estimate_sigma_g(data[,1], data[,2], n1, n2)
  sigma_c1 <- estimate_sigma_c1(data[,1], n1, sigma_g)
  sigma_c2 <- estimate_sigma_c2(data[,2], n2, sigma_g)
  #calculate mean and sd of conditional distribution
  mean <- data[,1]* (sqrt(n1*n2) *sigma_g) / (n1*sigma_g + n1*sigma_c1 +1) #vector
  sd <- sqrt(n2*sigma_g + n2*sigma_c2 + 1 - (n1*n2*sigma_g*sigma_g)/(n1*sigma_g + n1*sigma_c1 +1)) #scalar
  slope <- (sqrt(n1*n2) *sigma_g) / (n1*sigma_g + n1*sigma_c1 +1)
  
  #calculate replication rate across significant SNPs in initial study
  predicted_replication <- 0
  for(i in 1:nrow(data)){
    predicted_replication <- predicted_replication +  predict_replication(mean[i], sd, z)
    
  }
  
  return(c(predicted_replication/nrow(data), sigma_g, sigma_c1, sigma_c2, slope, sd))
}

calcReplication <- function(data, z){
  count <- 0
  for(i in 1:nrow(data)){
    if(abs(data[i,2])>abs(z)){
      count  <- count + 1
    }
  }
  return(c(count/nrow(data), count))
}

run_simulated_analysis <- function(t){
  #fix n
  # data[,3] <- max(data[,3])
  # data[,4] <- max(data[,4])
  
  #z-score for lower significance bound (upper bound is -1*lower bound)
  #need to adjust by factor of 1/2 for two sided test
  z <- qnorm(t/2) 
  
  
  #true replication rate
  r_true <- calcReplication(data, z)
  print(r_true[2])
  #plot winners curse
  #plotWinnersCurse(data)
  
  #predicted replication rate with no confounding
  print("Predicting with no confounding...")
  pr_no_confounding <- predict_no_confounding(data, z)
  count_no_confounding <- round(pr_no_confounding[1]*nrow(data))
  print(pr_no_confounding[2])
  print(count_no_confounding)
  
  print("Predicting with confounding...")
  #predicted replicatino rate with confounding
  pr_with_confounding <- predict_with_confounding(data, z)
  count_with_confounding <- round(pr_with_confounding[1]*nrow(data))
  print(pr_with_confounding[2:4])
  print(count_with_confounding)
  
  #plot conditional distribution
  #plot_conditional(data, pr_with_confounding[5], pr_with_confounding[6], pr_no_confounding[3], pr_no_confounding[4])
  
  #write text output
  #write.table(t(c(args[1], r_true, pr_no_confounding, count_no_confounding, pr_with_confounding, count_with_confounding)), file=paste0(args[2], ".txt"), row.names=F, col.names=F, quote=F)
  
  
  
}

run_simulated_analysis( t)

