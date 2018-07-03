power <- function(N, lamda, m) {
  alphaStar = 0.05/m
  power <- pnorm(qnorm(alphaStar/2)+lamda*sqrt(N))+1-pnorm(-qnorm(alphaStar/2)+lamda*sqrt(N))
  return(power)
}

args = commandArgs(trailingOnly=TRUE)
datafile = args[1]
outfile = args[2]
Data=read.csv(datafile,header=T,sep=" ")
#write.table( paste(t,p1,p2,sep=','), outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
output <- matrix(,nrow=0,ncol=5)
for(i in 1:nrow(Data)){
  N_GWAS <- as.numeric(as.character(Data[i,1]))
  N_Rep <- as.numeric(as.character(Data[i,2]))
  lambda <- as.numeric(as.character(Data[i,3]))
  m_GWAS <- as.numeric(as.character(Data[i,4]))
  m_Rep <- as.numeric(as.character(Data[i,5]))
  #probabilities of true positive for GWAS & Rep
  pr_tpg <- power(N_GWAS,lambda,m_GWAS)
  pr_tpr <- power(N_Rep,lambda,m_Rep)
  #case 1: false negative
  pr_fng <- 1-pr_tpg
  #case 2: both true positive
  pr_btp <- pr_tpg*pr_tpr
  #case 3: GWAS true positive but Rep false negative
  #pr_tpfn = pr_tpg-prbtp
  output <- rbind(output,c(N_GWAS,N_Rep,lambda,pr_fng,pr_btp))
}
write.table(output,outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
#N_Rep = as.numeric(args[2])
#lambda = as.numeric(args[3])
#m_GWAS = as.numeric(args[4])
#m_Rep = as.numeric(args[5])
#for(i in 1:5){
#  print(args[i])
#}
