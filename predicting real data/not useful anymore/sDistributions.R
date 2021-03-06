
#this will show us that doing the two different steps (picking a random lamda and then putting into another nromal
#r is the same thing as pulling from the conjugate prior formula...
#pulling lamda from normal distrubtion


#pulling lamda from normal distrubtion 
tempSd<-sqrt(1)
N=10000
sampleSize=10000
lamda <- rnorm(n=N, mean=0, sd=tempSd/sqrt(sampleSize))

#results<-data.frame(nrow(10000),ncol(3))
results<-matrix(nrow=10000, ncol=3)

for(i in 1:10000){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i]*sqrt(sampleSize),sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

#comparing test statistic 
s_noLamda <- rnorm(n=10000, mean=0, sd=sqrt(1+tempSd*tempSd))
d_theo <- density(s_noLamda)
d_prac <- density(results[,2])
plot(range(d_theo$x, d_prac$x), range(d_theo$y, d_prac$y), type = "n", xlab = "S-test statistics",
     ylab = "Density")
lines(d_theo, col = "red")
lines(d_prac, col = "blue")

