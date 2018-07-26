#reading in from command line 
args = commandArgs(trailingOnly=TRUE)
numArg=length(args)

for(i in 1:numArg){
  datafile = args[i]
  Data=read.csv(datafile,header=T,sep=" ")
  
  splitData <- split(Data, Data$trait.var)
  
  num<-length(splitData)
  
  for(i in 1:num){
    fileName<-paste(as.character(i),"_", datafile ,sep="")
    data_temp<-splitData[[i]]
    write.csv(data_temp, file<-fileName)
  }
  

}

