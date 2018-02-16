CI95 <- function(mean, sd, n) {
	se <- sd/sqrt(n)
	CI <- 1.96*se
	return(mean + c(-CI, CI))
	}