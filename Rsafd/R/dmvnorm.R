`dmvnorm` <-
function(x,mean=rep(0, d),cov=diag(d),sd,rho,d=2,sigma=cov,log = FALSE)
{
    # Compute multivariate normal density at x
    # mean may be a d-dimensional vector
    # cov should be a [d,d]-matrix, if supplied
    # sd and rho are considered only if cov is missing
    # sd should be a d-dimensional vector
    # rho should be scalar, if supplied
    # d is the number of columns; may be inferred from other arguments
    # method is a string giving the method SVD or Choleski used
    if(!missing(mean)) {
        if(!is.vector(mean))
            stop("mean must be a vector")
        d.mean <- length(mean)
    }
        if (is.vector(x))
            x <- matrix(x, ncol = length(x))
    if((!missing(cov)||!missing(sigma)) && (!missing(sd)||!missing(rho)))
        warning("Either (cov or sigma) or (sd and rho) should be given, NOT BOTH. cov will be used")
    if(!missing(cov)&&!missing(sigma))
        warning("Either cov or sigma should be given, NOT BOTH. cov will be used")
    if(missing(cov)&&!missing(sigma))
        cov <- sigma
    if(!missing(cov))
    {
        if(!is.matrix(cov) || diff(dim(cov)) || any(abs(cov - t(cov)) > sqrt(.Machine$double.eps)))
            stop("cov must be a square symmetric matrix")
        d.cov <- nrow(cov)
    }
    else
    {
        if(missing(sd)||missing(rho))
            stop("Both parameters sd and rho must be present if cov is missing")
        if(!missing(rho))
        {
            if(length(rho) != 1)
                stop("rho must be a number (i.e. have length 1)")
            if(any(rho < -1 | rho > 1))
                stop("rho must be between -1 and +1")
        }
        if(!missing(sd))
        {
            if(any(sd <= 0))
                stop("Negative or zero sd found")
            if(!is.vector(sd))
                stop("sd must be a vector")
            d.sd <- length(sd)
        }
    }
    # check that dimensions match
    if(!missing(cov) && (d.cov!=d.mean))
        stop("The dimensions of cov and mean do not match")
    if(!missing(cov))
        sigma <- cov
    else
        sigma <- (1-rho)*diag(sd*sd) + rho*outer(sd,sd,"*")
    distval <- mahalanobis(x, center = mean, cov = sigma)
        logdet <- sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
        logretval <- -(ncol(x) * log(2 * pi) + logdet + distval)/2
        if (log)
            return(logretval)
        exp(logretval)
}

