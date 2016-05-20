`p2Dnorm` <-
function(x,mean=rep(0, 2),std=rep(1,2),rho=0)
{
    # Compute bi-variate (i.e. d=2) normal cdf at x
    # mean may be a 2-dimensional vector
    # cov should be a [2,2]-matrix, if supplied
    # sd and rho are considered only if cov is missing
    # sd should be a 2-dimensional vector
    # rho should be scalar, if supplied

    if(!missing(mean)) {
        if(!is.vector(mean)||length(mean)!=2)
            stop("mean must be a 2D-vector")
    }
	if (is.vector(x))
		x <- matrix(x, ncol = length(x))
    
	if(!missing(rho))
	{
		if(length(rho) != 1)
                stop("rho must be a number (i.e. have length 1)")
		if(any(rho < -1 | rho > 1))
                stop("rho must be between -1 and +1")
	}
	if(!missing(std))
	{
		if(any(std <= 0))
			stop("Negative or zero std found")
		if(!is.vector(std)||length(std)!=2)
                stop("std must be a 2D-vector")
	}
		
	ff <- function(z)
	{
		m21 <- mean[2] + rho*std[2]*(z-mean[1])/std[1]
		s21 <- std[2]*sqrt(1-rho*rho)
		pnorm(x[2], mean=m21, sd=s21)*dnorm(z, mean=mean[1], sd=std[1])
	}
	integrate(ff, lower=-Inf, upper=x[1])$value
}

