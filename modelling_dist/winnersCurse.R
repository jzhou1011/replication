library(tidyverse)

#here we are plotting are plotting the test statistic against the lamda that generated it. 
#here we can see that the.... NEED TO UNDERSTAND THE DIFFERENT LINES/SD/mean

#pulling lamda from normal distrubtion 
tempSd<-sqrt(3)
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=tempSd/sqrt(sampleSize))


results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i]*sqrt(sampleSize),sd=1)
}


results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

true_lambda <- function(lambda){
  lambda*sqrt(sampleSize)
}

#den_init_lambda <- density(results.data$lamda)
#plot(den_init_lambda)

#plot(results.data[,c(1,2)])
s1VSlamda<-ggplot(data = results.data, mapping = aes(x = lamda, y = s1,colour="red")) +geom_point()
s1VSlamda <- s1VSlamda + stat_function(fun=true_lambda,geom="line",colour="blue")
ggsave(filename="s1_VS_lambda.jpg")

#den_lambda = density(filter(results.data,s1>5.2 | s1<(-5.2) && lamda>0)$s1)
#den_lambda = density(filter(results.data,s1>5.2 | s1<(-5.2))$lamda)
#plot(den_lambda)

sig_stats<-ggplot(data = filter(results.data, s1>5.2 | s1<(-5.2)), mapping = aes(x = lamda, y = s1)) +geom_point()
ggsave(filename="lambda_significant_stats.jpg")

#contingency table comparing two test statistics generated from the same lambda 
#here we can see the replication probelm 
#s1 is the colums 
#s2 is the rows
cont_table = matrix(c(0,0,0,0),nrow=2,ncol=2)
for (i in 1:N){
  boolS1=(results[i,2] > 5.2 | results[i,2] < -5.2)
  boolS2=(results[i,3] > 5.2 | results[i,3] < -5.2)
  if (boolS1 && boolS2){
    cont_table[2,2] <- cont_table[2,2]+1
  }else if (!boolS1 && boolS2){
    cont_table[2,1] <- cont_table[2,1]+1
  }else if (boolS1 && (!boolS2)){
    cont_table[1,2] <- cont_table[1,2]+1
  }else{
    cont_table[1,1] <- cont_table[1,1]+1
  }
}

