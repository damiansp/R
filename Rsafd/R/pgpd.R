`pgpd` <-
function(x, m = 0, lambda = 1, xi = 0)
{   
    k <- xi
    if (!SHAPE.XI) k <- -xi
	if ((length(m)!=length(lambda))|(length(m)!=length(xi))|(length(xi)!=length(lambda)))
		stop("m, lambda and xi should have the same lengths")
	n <- length(m)
	if ((n > 1) & (length(x) != n))
		stop("When vectors of lengths > 1, m, lambda and xi should have the same length as x")
	val <- rep(Inf, length(x))  
	if (n==1)
	{
		if (k==0) {
			val <- 1. - exp(-(x - m)/lambda)
			val[x<m] <- 0
		}
		else 
		{ 
			val <- 1. + k * (x - m)/lambda
			if (k>0) {
				val[x<=m] <- 0
				val[x>m] <- 1 - val[x>m]^(-1/k)
			}
			else {
				val[x<=m | x >= m-lambda/k] <- 0
				val[x>m & x < m-lambda/k] <- 1 - val[x>m & x < m-lambda/k]^(-1/k)
			}
		}
	}
	else
	{
		K0 <- (k==0)
		KPOS <- (k>0)
		KNEG <- (k<0)
		val[K0] <- 1. - exp(-(x[K0] - m[K0])/lambda[K0])
		val[(x < m) & K0] <- 0
		val[!K0] <- 1. + k[!K0] * (x[!K0] - m[!K0])/lambda[!K0]
		val[KPOS & (x<=m)] <- 0
		val[KPOS & (x>m)] <- 1 - val[KPOS & (x>m)]^(-1/k[KPOS & (x>m)])
		val[KNEG &(x<=m | x >= m-lambda/k)] <- 0
		val[KNEG & x>m & x < m-lambda/k] <- 1 - val[KNEG & x>m & x < m-lambda/k]^(-1/k[KNEG & x>m & x < m-lambda/k])
	}
    val
}

