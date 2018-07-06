
#pulling lamda from normal distrubtion 
tempSd<-sqrt(0.5)
N=10000
sampleSize=50
lamda <- rnorm(n=N, mean=0, sd=tempSd)

#results<-data.frame(nrow(10000),ncol(3))
results<-matrix(nrow=N, ncol=3)

for(i in 1:N){
  results[i,1] = lamda[i]
  results[i,c(2,3)] <- rnorm(n=2,mean=lamda[i]*sqrt(sampleSize),sd=1)
}

results.data=as.data.frame(results)
colnames(results.data)=c("lamda", "s1", "s2")

#comparing test statistic 
s_noLamda <- rnorm(n=N, mean=0, sd=sqrt(1+tempSd*tempSd))
s_noLamda<-as.data.frame((s_noLamda))
results.data$s3<-s_noLamda$`(s_noLamda)`
  

#plot(results.data[,c(1,2)])
s1VSlamda<-ggplot(data = results.data, mapping = aes(x = lamda, y = s1)) +geom_point() + xlim(-5,5)

winnersCurse<-ggplot(data = filter(results.data, s1>5.2 | s1<(-5.2)), mapping = aes(x = lamda, y = s1)) +geom_point() + xlim(-5,5)
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

