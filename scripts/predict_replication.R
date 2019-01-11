args = commandArgs(trailingOnly=TRUE)
filename<-args[1]

data<-read.table(filename, header=T)

#estimate variance components
estimate_sigma_g <- function(s1, s2, n1, n2){
	return (cov(s1, s2) / sqrt(n1*n2) )
}

estimate_sigma_c1 <- function(s1, n1, sigma_g){
	num <- var(s1) - n1*sigma_g - 1
	return(num/n1)
}

estimate_sigma_c2 <- function(s2, n2, sigma_g){
	num <- var(s2) - n2*sigma_g -1
	return(num/n2)
}

#calculate conditional probability of s2 > t | s1 = x under model with no confounding
calc_conditional_no_confounding <- function(s1, n1, n2, sigma_g, z){
	mean <- s1 * (sqrt(n1*n2) *sigma_g)/(n1*sigma_g + 1) 
	sd <- 1 + (n2*sigma_g)/(n1*sigma_g +1)

	lower <-  pnorm(z, mean, sd)
	upper <- 1- pnorm(-z, mean, sd)
	return(lower + upper)
}

#calculate conditional probability of s2 > t | s1 = x under model with confounding
calc_conditional_with_confounding <- function(s1, n1, n2, sigma_g, sigma_c1, simga_c2, z){
	mean <- s1* (sqrt(n1*n2) *sigma_g) / (n1*sigma_g + n1*sigma_c1 +1)
	sd <- n2*sigma_g + n2*simga_c2 + 1 - (n1*n2*sigma_g*sigma_g)/(n1*sigma_g + n1*sigma_c1 +1)

	lower <-  pnorm(z, mean, sd)
	upper <- 1-pnorm(-z, mean, sd)
	return(lower + upper)

}

#calculate predicted replication rate under model with no confounding
predict_no_confounding <- function(data, z){
	#currently assume sample size is same for all variants
	n1 <- data[1,3]
	n2 <- data[1,4]
	sigma_g <- estimate_sigma_g(data[,1], data[,2], n1, n2)
	print(sigma_g)
	
	predicted_replication <- 0
	for(i in 1:nrow(data)){
		predicted_replication <- predicted_replication +  calc_conditional_no_confounding(data[i,1], n1, n1, sigma_g, z)

	}

	return(predicted_replication/nrow(data))
}

#calculate predicted replication rate under model with confounding
predict_with_confounding <- function(data, z){
	n1 <- data[1,3]
	n2 <- data[1,4]
	sigma_g <- estimate_sigma_g(data[,1], data[,2], n1, n2)
	sigma_c1 <- estimate_sigma_c1(data[,1], n1, sigma_g)
	sigma_c2 <- estimate_sigma_c2(data[,2], n2, sigma_g)

	print(c(sigma_g, sigma_c1, sigma_c2))
	predicted_replication <- 0
	for(i in 1:nrow(data)){
		 predicted_replication <- predicted_replication +  calc_conditional_with_confounding(data[i,1], n1, n2, sigma_g, sigma_c1, sigma_c2, z)

	}

	return(predicted_replication/nrow(data))
}

calcReplication <- function(data, z){
	count <- 0
	for(i in 1:nrow(data)){
		if(abs(data[i,2])>abs(z)){
			count  <- count + 1
		}
	}
	return(count/nrow(data))
}

run_mouse_analysis <- function(data){
	bonferonni <- 0.05/nrow(data)
	#z-score for lower significance bound (upper bound is -1*lower bound)
	#need to adjust by factor of 1/2 for two sided test
	z <- qnorm(0.05/2) 
	print(z)

	#variants that are significant in initial replication study
	sig <- subset(data, abs(data[,1])> abs(z))
	print(dim(sig))


	#true replication rate
	r_true <- calcReplication(data, z)
	print(r_true)

	#predicted replication rate with no confounding
	print("Predicting with no confounding...")
	pr_no_confounding <- predict_no_confounding(data, z)
	print(pr_no_confounding)

	print("Predicting with confounding...")
	#predicted replicatino rate with confounding
	pr_with_confounding <- predict_with_confounding(data, z)
	print(pr_with_confounding)



	write.table(c(r_true, pr_no_confounding, pr_with_confounding), file=args[2], row.names=F, col.names=F, quote=F)



}

run_mouse_analysis(data)
	
