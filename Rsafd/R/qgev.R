`qgev` <-
function(p,  m=0, lambda = 1, xi = 0) 
{
    k <- xi
    if (!SHAPE.XI) k <- -xi
	if ((length(m)!=length(lambda))|(length(m)!=length(xi))|(length(xi)!=length(lambda)))
		stop("m, lambda and xi should have the same lengths")
	n <- length(m)
	if ((n > 1) & (length(p) != n))
		stop("When vectors of lengths > 1, m, lambda and xi should have the same length as p")
    if (sum(p <= 0 | p >= 1)>0) 
        warning("Argument of qgev should be between 0 and 1, NA will be returned in other cases")
	val <- rep(Inf, length(p))  
	if (n==1)
	{
		if (k==0)  
			val <- m - lambda * log(-log(p))
		else 
			val <- m - lambda/k * ( 1 - (-log(p))^(-k))
	}
	else
	{
		K0 <- (k==0)
		val[K0] <- m[K0] - lambda[K0] * log(-log(p[K0]))
		val[!K0] <- m[!K0] - lambda[!K0]/k[!K0] * ( 1 - (-log(p[!K0]))^(-k[!K0]))
	}
    val [p < 0 | p > 1] <- NA
    val
}

