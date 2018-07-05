args = commandArgs(trailingOnly=TRUE)
datafile = args[1]

Data=read.csv(datafile,header=F,sep=" ")
colnames(Data) <- c("N_GWAS","N_Rep","lambda","Pr_true_pos_GWAS","Pr_true_pos_GWAS_Rep")

#installing packages
#install.packages('ggplot2')
library(ggplot2)

#install.packages("reshape2")
library(reshape2)

for( i in 1:5){
  power_g <- ggplot(subset(Data,lambda==i*0.01),aes(x=N_GWAS,y=N_Rep))+geom_tile(aes(fill = Pr_true_pos_GWAS))
  power_g+scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.1), low="white",high="steelblue")
  ggsave(paste("power-",as.character(i*0.01),".jpg",sep=""))
  both_g <- ggplot(subset(Data,lambda==i*0.01),aes(x=N_GWAS,y=N_Rep))+geom_tile(aes(fill = Pr_true_pos_GWAS_Rep))
  both_g + scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.1), low="white",high="steelblue")
  ggsave(filename=paste("prb_btp-",as.character(i*0.01),".jpg",sep=""))
}