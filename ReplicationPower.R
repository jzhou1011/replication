
#true positive in GWAS

# N GWAS 1,000~50,000 step:1000
# N rep 100~5,000 step:100
# lambda 0~0.05 step:0.01
# m GWAS 100,000
# m rep 1
# 
# 5 graphs, 2500 data points each

NGWAS<-seq(from = 1000, to = 50000, by=1000)
N<-seq(from = 100, to=5000, by=100)
lamda<-seq(from=0, to=0.05, by=0.05)
mGWAS=100000
mRep=1

power <- function(N, lamda, m) {
  alphaStar = 0.05/m
  power <- pnorm(qnorm(alphaStar/2)+lamda*sqrt(N))+1-pnorm(-qnorm(alphaStar/2)+lamda*sqrt(N))
  return(power)
}



