library(ggplot2)

# power <- function(lamda) {
#   N=10000
#   power <- pnorm(5.2+lamda*sqrt(N))+1-pnorm(-5.2+lamda*sqrt(N))
#   return(power)
# }

power_1k <- function(lamda) {
  alphaStar = .0000001
  N=1000
  power <- pnorm(qnorm(alphaStar/2)+lamda*sqrt(N))+1-pnorm(-qnorm(alphaStar/2)+lamda*sqrt(N))
  return(power)
}

powerSquared_1k <- function(lamda){
  power_1k(lamda)*power_1k(lamda)
}


power_10k <- function(lamda) {
  alphaStar = .0000001
  N=10000
  power <- pnorm(qnorm(alphaStar/2)+lamda*sqrt(N))+1-pnorm(-qnorm(alphaStar/2)+lamda*sqrt(N))
  return(power)
}

powerSquared_10k <- function(lamda){
  power_10k(lamda)*power_10k(lamda)
}

power_100k <- function(lamda) {
  alphaStar = .0000001
  N=100000
  power <- pnorm(qnorm(alphaStar/2)+lamda*sqrt(N))+1-pnorm(-qnorm(alphaStar/2)+lamda*sqrt(N))
  return(power)
}

powerSquared_100k <- function(lamda){
  power_100k(lamda)*power_100k(lamda)
}


plot<-ggplot(data.frame(x=c(0,0.11)), aes(x=x))+xlab("Lamda")+ylab("Probability")+
    stat_function(fun=powerSquared_1k, geom="line", aes(colour="Replication, N=1k"))+
    stat_function(fun=power_1k, geom="line", aes(colour="Power, N=1k"))+
    stat_function(fun=powerSquared_10k, geom="line", aes(colour="Replication, N=10k"))+
    stat_function(fun=power_10k, geom="line", aes(colour="Power, N=10k"))+
    stat_function(fun=powerSquared_100k, geom="line", aes(colour="Replication, N=100k"))+
    stat_function(fun=power_100k, geom="line", aes(colour="Power, N=100k"))+
    ggtitle("         Power and the probability of replication")+
    xlim(0,0.3)

ggsave(filename = "Power&BothReplication_ProbabilityCurve.jpg")


