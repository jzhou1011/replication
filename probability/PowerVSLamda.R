library(ggplot2)

# power <- function(lamda) {
#   N=10000
#   power <- pnorm(5.2+lamda*sqrt(N))+1-pnorm(-5.2+lamda*sqrt(N))
#   return(power)
# }

power <- function(lamda) {
  alphaStar = .00000001
  N=10000
  power <- pnorm(qnorm(alphaStar/2)+lamda*sqrt(N))+1-pnorm(-qnorm(alphaStar/2)+lamda*sqrt(N))
  return(power)
}

powerSquared <- function(lamda){
  power(lamda)*power(lamda)
  }

plot<-ggplot(data.frame(x=c(0,0.05)), aes(x=x))+xlab("Lamda")+ylab("Probability")+
    stat_function(fun=powerSquared, geom="line", aes(colour="GWAS=1, Rep=1"))+
    stat_function(fun=power, geom="line", aes(colour="The Power"))+scale_colour_manual(values=c("blue", "red"))+
    ggtitle("                  Probability of the power and two true positives")

ggsave(filename = "Power&BothReplication_ProbabilityCurve.jpg")


