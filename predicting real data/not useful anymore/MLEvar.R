#MLE sigmasqaured 

#filename<-"./files/1_19557161_data_upbuilt_filtered_upbuilt.csv" #average #6 vs 24.31 

args = commandArgs(trailingOnly=TRUE)
filename<-args[1]
data<-read.csv(filename, sep=",")


#find test statsitic 
results.data<-data.frame(data$beta.disc, data$se.disc)
results.data$s1<-(data$beta.disc)/(data$se.disc)
results.data$s2<-(data$beta.rep)/(data$se.rep)


thresh<-data$p.thresh[1]
thresholdZscore<-qnorm(thresh,lower.tail =FALSE)
M<-0.05/thresh
MSig<-nrow(data)
traitVar<-data$trait.var[1]

var<-5

MLE<-function(var){
  estimate<-1
  for(i in results.data$s1){
    estimate<-estimate*dnorm(i, mean=0, sd=sqrt(var))
  }
  estimate<-estimate*(pnorm(thresholdZscore, mean=0, sd=sqrt(var)))^(M-MSig)
}

max<-0
maxVar<-0;
# for(i in seq(from=1, to=traitVar, by=.01)){
#   temp<-MLE(i)
#   if (temp>max) {
#     max<-temp
#     maxVar<-i
#   }
# }

for(i in seq(from=1, to=100, by=.001)){
  temp<-MLE(i)
  if (temp>max) {
    max<-temp
    maxVar<-i
  }
}

#formating 
maxVar<-formatC(maxVar, width = 4, format="f")
max<-formatC(max, width = 4, format="f")
#results <- paste(as.character(maxVar),as.character(max),"\n",sep=" ")
results <- paste(as.character(maxVar),"\n",sep="")
cat(results)
