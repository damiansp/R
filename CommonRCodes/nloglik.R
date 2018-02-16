nloglik <- function(p, y, n) {
	loglik(p, y, n) - loglik(y/n, y, n)
	}