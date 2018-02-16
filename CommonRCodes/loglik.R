loglik <- function(p, y, n){
	lchoose(n, y) + y*log(p) + (n - y)*log(1-p)
	}