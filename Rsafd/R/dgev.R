`dgev` <-
function(x,  m=0, lambda = 1, xi = 0) 
{    
    k <- xi
    if (!SHAPE.XI) k <- -xi
	if ((length(m)!=length(lambda))|(length(m)!=length(xi))|(length(xi)!=length(lambda)))
		stop("m, lambda and xi should have the same lengths")
	n <- length(m)
	if ((n > 1) & (length(x) != n))
		stop("When vectors of lengths > 1, m, lambda and xi should have the same length as x")
	val <- rep(Inf, length(x))  
	uu <- exp(-(x-m)/lambda)
	vv <- (1+(x-m)*k/lambda)^(-1/k -1)
	if (n==1)
	{
		if (k==0)
            val <- uu/lambda*exp(-uu) 
		else
			val <-   vv/lambda*exp(-vv*(1+(x-m)*k/lambda))
	}
	else
	{
		K0 <- (k==0)
		KN0 <- !K0
		val[K0] <- uu[K0]/lambda[K0]*exp(-uu[K0])
		val[KN0] <- vv[KN0]/lambda[KN0]*exp(-vv[KN0]*(1+(x[KN0]-m[KN0])*k[KN0]/lambda[KN0]))
	}
    val
}
