##Logit transformation:  transforms (-Inf, Inf) <-- (0, 1)

logit <- function(x) {
	log(x/(1-x))
	}
	
ilogit <- function(x) {
	exp(x) / (1+ exp(x))
	}