#estimating sigma sqaured g

#simulating test statistics 
sigma<-sqrt(2)
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=sigma)

results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i],sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

#calculating the variance of s1 to estimate sigma squared g 
#ask why this is different than R's 
compute_var<-function(s1_v,mean){
  sum<-0
  for(s1 in s1_v){
    sum<-(s1-mean)^2+sum
  }
  var<-sum/N
  return(var)
}

#compute_var(results.data$s1, mean(results.data$s1))


#confidence interval 
#right now assuming 95% confidence interval

#Large degrree of freedom 
var<-var(results.data$s1)
pred_sig_sq <- var-1

rnorm(var,sqrt(2*var))
criticalValue=1.959964
chiSquaredUpper<-((criticalValue+sqrt(2*(N-1)-1))^2)/2
chiSquaredLower<-((-criticalValue+sqrt(2*(N-1)-1))^2)/2


compute_confidence<-function(var){
  upperBound<-((N-1)*var)/chiSquaredLower
  lowerBound<-((N-1)*var)/chiSquaredUpper
  confidence<-c(lowerBound,upperBound)
  return(confidence)
  }

compute_confidence(compute_var(results.data$s1, mean(results.data$s1)))

#now using s1 and s2 to estimate 
#distribution of the average (s1+s2)/2

#different when using our own function from using r function var
# var_avg <- 0.25*compute_var(results.data$s1, mean(results.data$s1))+
#                  0.25*compute_var(results.data$s2, mean(results.data$s2))+
#                   0.5*cov(results.data$s1,results.data$s2)
var_avg <- 0.25*var(results.data$s1)+
  0.25*var(results.data$s2)+
  0.5*cov(results.data$s1,results.data$s2)
pred_sig_sq2<-var_avg-0.5

#confidence Interval 
compute_confidence(var_avg)



