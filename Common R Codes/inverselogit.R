#Inverse logit function: tranforms (0, 1) --> (-Inf, Inf)

ilogit <- function(x) {
	exp(x)/(1+exp(x))
	}