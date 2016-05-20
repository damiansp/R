
kdest <- function(X,Y,H,N=256,lims=c(range(X),range(Y)),COND=0,PLOT=T,XNAME="X",YNAME="Y")
{
    # Compute from a bivariate sample of values of (X,Y) on a regular NxN grid
    # the values of the joint density of (X,Y) if COND=0, a joint estimate of the
    # conditional density of Y given X if COND=1 or
    # a joint estimate of the conditional density of X given Y if COND=2
    # Use a 2D-Gaussian kernel and condition by division by the marginal obtained by integration

    LX <- length(X)
    if(length(Y) != LX)
        stop("Vectors must have same lengths"
            )
    GRIDX <- seq((5 * lims[1] - lims[2])/4, (5 * lims[2] - lims[1])/4, length = N)
    DELTAX <- (3 * (lims[2] - lims[1]))/(2 * 256)
    GRIDY <- seq((5 * lims[3] - lims[4])/4, (5 * lims[4] - lims[3])/4, length = N)
    DELTAY <- (3 * (lims[4] - lims[3]))/(2 * 256)
    if(missing(H))
        H <- c(bandwidth.nrd(X),bandwidth.nrd(Y))
    H <- H/4
    DX <- outer(GRIDX, X, "-")/H[1]
    DY <- outer(GRIDY, Y, "-")/H[2]
    Z <- matrix(dnorm(DX), N, LX) %*% t(matrix(dnorm(DY), N, LX))/(LX * H[1] * H[2])
    cat("Bandwidths values used: ", H,"\n")
    MARGX <- outer(DELTAX * DELTAY * apply(Z,1,sum),rep(1,N),"*")
    if(COND == 1)
        Z[MARGX > 1e-005] <- Z[MARGX>1e-005]/MARGX[MARGX>1e-005]
    MARGY <- outer(rep(1, N), DELTAX*DELTAY*apply(Z, 2, sum),"*")
    if(COND == 2)
        Z[MARGY > 1e-005] <- Z[MARGY >1e-005]/MARGY[MARGY >1e-005]
    if(PLOT == T) {
        persp3d(GRIDX, GRIDY, Z, aspect=c(1, 1, 0.5), col = "lightblue",xlab = XNAME, ylab = YNAME, zlab = "Density")
    }
    list(deltax=DELTAX,deltay=DELTAY,gridx=GRIDX,gridy=GRIDY,z=Z)
}

rmvnorm <- function(n,mean=rep(0, d),cov=diag(d),sd,rho,d=2,sigma=cov,method=c("svd", "chol"))
{
    # Generate multivariate random normal samples
    # n is the number of observations
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
    method <- match.arg(method)
    if (method == "svd") {
        ev <- eigen(sigma, sym = TRUE)$values
        if (!all(ev >= -sqrt(.Machine$double.eps) * abs(ev[1]))) {
            warning("sigma is numerically not positive definite")
        }
        sigsvd <- svd(sigma)
        retval <- t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
    }
    if (method == "chol") {
        retval <- chol(sigma, pivot = T)
        o <- order(attr(retval, "pivot"))
        retval <- retval[, o]
    }
    retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
    retval <- sweep(retval, 2, mean, "+")
    retval
}

dmvnorm <- function(x,mean=rep(0, d),cov=diag(d),sd,rho,d=2,sigma=cov,log = FALSE)
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

l1fit <- function(x,y, intercept = TRUE)
{
    warning("l1fit() in R is just a wrapper to rq(). Use that instead!")
    if(intercept) rq(y ~ x, tau = 0.5)
    else rq(y ~ x - 1, tau = 0.5)
}

eda.shape <- function(x)
{
    par(mfrow = c(2, 2))
    hist(x)
    boxplot(x)
    iqd <- summary(x)[5] - summary(x)[2]
    plot(density(x, width = 2 * iqd), xlab = "x", ylab = "", type = "l")
    qqnorm(x)
    qqline(x)
    par(mfrow = c(1, 1))
}
