library(ggplot2)
true_rep <- 519 
palmer <- 610
wc <- 893
wc_c <- 550

df <- data.frame(correction=c("Palmer", "WC", "WC+Confounding"),
                 len=c(palmer, wc, wc_c))

par(mar=c(5.1,6.1,4.1,6.1))
barplot(c(palmer, wc, wc_c), names.arg=c("Palmer", "WC", "WC+Confounding"), ylab="Predicted Number of Variants", cex.lab=2, cex.axis=2, cex=2)
abline(h=519, lty=3)
legend("topright", inset=c(0,0), legend=c("True replication"), lty=3, cex=1)

