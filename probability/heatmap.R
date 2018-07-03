args = commandArgs(trailingOnly=TRUE)
datafile = args[1]

Data=read.csv(datafile,header=F,sep=" ")
colnames(Data) <- c("N_GWAS","N_Rep","lambda","power","both_positive")

#installing packages
#install.packages('ggplot2')
library(ggplot2)

#install.packages("reshape2")
library(reshape2)

for( i in 1:5){
  temp_graph <- ggplot(subset(Data,lambda==i*0.01),aes(x=N_GWAS,y=N_Rep))+geom_tile(aes(fill = power))
  ggsave(paste("power-",as.character(i*0.01),".jpg",sep=""))
  graph_temp <- ggplot(subset(Data,lambda==i*0.01),aes(x=N_GWAS,y=N_Rep))+geom_tile(aes(fill = both_positive))
  ggsave(filename=paste("prb_btp-",as.character(i*0.01),".jpg",sep=""))
}