##########
# Univariate Analysis and GPDs


SHAPE.XI <- TRUE

block.max <- function(data, overlap=0, nb.blocks=NA, block.size=100, method = "ml")
{
    # divide the vector data into (possibly overlapping) blocks
    # estimate the shape parameter of the upper tail with
    # the GEV shape parameter of the sample of block maxima

    if ((!is.vector(data))||(!is.numeric(data)))
        stop("data should be a numeric vector")
    if (overlap>50)
        stop("overlap should be an integer smaller than 50!")
    if (block.size<100)
        stop("block.size should be an integer greater than 100!")
    n <- length(data)
    overlap <- as.integer(overlap)
    block <- block.size-overlap
    NB <- as.integer(n/block)
    if (is.na(nb.blocks))     
        nb.blocks <- NB
    nb.blocks <- min(nb.blocks,NB)
    block.index <- 0:(nb.blocks-1)
    B <- rev(n - block.index*block)
    A <- B + 1 - block.size
    GOOD <- A>0
    nb.blocks <- sum(GOOD)
    A <- A[GOOD]
    B <- B[GOOD]
    M <- rep(0,nb.blocks)
    for (I in 1:nb.blocks) M[I] <- max(data[A[I]:B[I]])
    if(method=="lmom")
            par <- gev.lmom(M)$param.est
    else    if(method=="ml")
                par <- gev.ml(M)$param.est
            else stop("method should be either lmom or ml!")
    out <- list(n = length(data), data = sort(data),
        method=method, nb.blocks=nb.blocks, overlap=overlap,
        interval.a = A, interval.b = B,
        param.est = par)
    oldClass(out) <- "block.max"
    out
}

dgev <- function(x,  m=0, lambda = 1, xi = 0) 
{    
    k <- xi
    if (!SHAPE.XI) k <- -xi

    if (k==0) {  
            uu <- exp(-(x-m)/lambda)
            val[k == 0] <- uu[k==0]/lambda[k==0]*exp(-uu[k==0])
    }
    else {
        uu <- (1+(x-m)*k/lambda)^(-1/k -1)
        val <-   uu/lambda*exp(-uu*(1+(x-m)*k/lambda))
    }
    val
}

dgpd <- function(x, m = 0, lambda = 1, xi = 0) 
{
    k <- xi
    if (!SHAPE.XI) k <- -xi

    if (k==0) {
        val <- exp(-(x - m)/lambda)/lambda
        val[x < m] <- 0
    }
    else { 
        val <- 1. + k * (x - m)/lambda
        if (k>0) {
            val[x<=m] <- 0
            val[x>m] <- val[x>m]^(-1/k-1)/lambda
        }
        else {
            val[x<=m | x >= m-lambda/k] <- 0
            val[x>m & x < m-lambda/k] <- val[x>m & x < m-lambda/k]^(-1/k-1)/lambda
        }
    }
    val
}

estimate.mix.gev <- function(sample, init.est = NA, sampLmom = NA, epsilon = 1e-5) 
{
         lmomest <- init.est
         assign("tempX",sample,pos= 1)
         assign("tempN",length(sample), pos =1)

         if(is.na(sampLmom[1])) sampLmom <- sample.LMOM(tempX)      
          
         if (is.na(lmomest[1])) { 
                 lmomest <- gev.lmom(sample.LMOM(tempX))$param.est
                 if (SHAPE.XI)  lmomest[3] <- -lmomest[3]
         }
         assign("templambda1", as.vector(sampLmom[1]) , pos = 1)
         negative.log.likelihood <- function(theta13) {
            k <- theta13[2]
            lambda <- theta13[1]
            m <- templambda1 - lambda/k*(1 - gamma(k+1))

            xsc <- 1 - k*(tempX-m)/lambda
            if (sum(xsc < 0) > 0 | lambda < 0) ll <- 10^10
            else { ll <- -tempN*log(lambda) - sum(xsc^(1/k)) + (1/k - 1)*sum(log(xsc)) }
            
            -ll   
         }  
    
         lambdak <- c(lmomest[2], lmomest[3])
         fit <- nlm(negative.log.likelihood, lambdak, steptol = epsilon)
         if (fit$code>3) { warning("MIX1 Method for GEV did not converge") }

         theta13 <- fit$estimate
         k <- theta13[2]
         lambda <- theta13[1]
         m <- templambda1 - lambda/k*(1 - gamma(k+1))

         paramest <- c(m,lambda,k)        
         names(paramest) <- c("m","lambda","k")
         if (SHAPE.XI) { paramest[3] <- -paramest[3]    
         names(paramest) <-  c("m","lambda","xi")
         }
        
         val <- list(param.est = paramest, convereged = as.logical((fit$code<4)))
         val
}

gev.lmom <- function(lmom)
{

        if (length(lmom) > 4) {
            message <- "It looks like the argument of gev.lmom is a sample and not sample L-moments."
            message <- paste(message, " Parameter estimation proceeds with this assumption,")
            message <- paste(message, " computing the L-moments from this sample and then,")
            message <- paste(message, " applying the function gev.lmom to this set of L-moments")
            warning(message)
            lmom <- sample.LMOM(lmom)
        }
    
        epsilon <- 1e-6
       
        c1 <- 7.817740; c2 <- 2.930462; c3 <- 13.641492; c4 <- 17.206675;
        eu <- -0.577215664901532861;
        z0 <- log(2.0)/log(3.0);

        t3 <- lmom[3];
        
        if(is.na(lmom[3]) | lmom[3] > 1)
            { stop(" The L-skewness is not specified or greater than 1 " ); }
        if(is.na(lmom[2]) | lmom[2] < 0 )
            { stop("Negative second L-moment!" ); }
       
        z <- 2.0/(3.0+t3) - z0;
         
        # Initial guess for k
        g <- z*(c1+z*(c2 + z*(c3 + z*c4)));

        # don't solve the equation, since the approximation
        # is good for  -0.1 < k < 0.5
        dosolve <- T        
        if ( t3 >= -0.1 & t3 <= 0.5) {dosolve <- F}
        if (dosolve & t3 < -0.9) {g <- 1.0 - log(1.0+t3)/log(2.0)}
        
        t0 <- 1;
        t00 <- (t3 + 3.0)/2.0
        assign("t0",t00, pos = 1)
        
        equation.error <- function(g) {
              t <- (1.0 - 3.0^(-g))/(1.0 - 2.0^(-g)) - t0
              abs(t)
        }
    
        if (dosolve)
        {   
           fit <- nlm(equation.error, g, iterlim = 100, steptol = epsilon)
           if ((fit$code==3)|(fit$code==4)|(fit$code==5))
                warning("LMOM estimation for GEV: Solution of the equation for k did not converge")
           g <- fit$estimate
        }
        paramest <- c(NA,NA,NA)
        names(paramest) <- c("m","lambda","k")
       
        paramest[3] <- g;
        gam <- gamma(1.0+g);

        # I changed the original order of EVANESCE: location is now first, scale second           
        paramest[2] <- lmom[2] *g / (gam*(1.0-2.0^(-g)));
        paramest[1] <- lmom[1] - paramest[2] *(1.0 - gam)/g;
        
        if (SHAPE.XI) 
        { 
            paramest[3] <- -paramest[3]
            names(paramest) <-  c("m","lambda","xi")
        }
        
        val <- list(param.est = paramest)
        val
}

gev.ml <- function(sample, init.est = NA, epsilon = 1e-5) 
{
        lmomest <- init.est
        assign("tempX",sample,pos= 1)
        assign("tempN",length(sample), pos =1)

        if (is.na(lmomest[1])) 
        { 
            lmomest <- gev.lmom(sample.LMOM(tempX))$param.est
            if (SHAPE.XI)  lmomest[3] <- -lmomest[3]
        }
        negative.log.likelihood <- function(theta)
        {
            k <- theta[3]
            m <- theta[1]
            lambda <- theta[2]

            xsc <- 1 - k*(tempX-m)/lambda
            ll <- NA 
            if (sum(xsc < 0) > 0 | lambda < 0) { ll <- NA}
            else  ll <- -tempN*log(lambda) - sum(xsc^(1/k)) + (1/k - 1)*sum(log(xsc))
            
            -ll   
        }  
        
        fit <- nlm(negative.log.likelihood, lmomest, iterlim = 100, steptol = epsilon)
        if (fit$code>3) 
            warning("Maximum Likelihood Method for GEV did not converge")

        paramest <- fit$estimate
        names(paramest) <- c("m","lambda","k")
        if (SHAPE.XI)
        {
            paramest[3] <- -paramest[3]
            names(paramest) <-  c("m","lambda","xi")
        }

        val <- list(param.est = paramest, converged = as.logical(fit$code<4))
        val
}

gpd.1p <- function(x, obj, linear = T)
{
    if(is(x, "timeSeries")) {
        x <- seriesData(x)
    }
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    x.orig <- x
    x <- sort(x)
    if(oldClass(obj) != "gpd") {
        stop("Wrong object. Object has to be of class gpd")
    }
    n <- length(x)
    k <- obj$upper.par.ests["xi"]
    if(!SHAPE.XI) {
        k <-  - k
    }
    ndata <- obj$n
    u <- obj$upper.thresh
    pp <- (ppoints(obj$data))
    small <- x <= u
    val <- vector(length = n, mode = "numeric")
    xsm <- as.double(x[small])
    nsm <- as.integer(length(xsm))
    lsmallpts <- as.integer(sum(obj$data <= u) + 1)
    smallpts <- as.double(obj$data[1:lsmallpts])
    oldind <- vector(length = nsm, mode = "numeric")
    oldind[1:nsm] <- -1
    indB <- .C("empirfunc",
        xsm,
        smallpts,
        nsm,
        lsmallpts,
        as.integer(oldind))[[5]]
    indB <- indB + 1
    lvalB <- obj$data[indB]
    indL <- indB - 1
    indL[indL == 0] <- NA
    lvalS <- obj$data[indL]
    lvalS[is.na(lvalS)] <- (x[small])[is.na(lvalS)]
    lvalB[is.na(lvalB)] <- 0
    pvalB <- pp[indB]
    pvalS <- pp[indL]
    #lvalS[is.na(lvalS)] <- 0
    #lvalB[is.na(lvalB)] <- 0
    pvalS[is.na(pvalS)] <- 0
    pvalB[is.na(pvalB)] <- 0
    if(linear) {
        val[small] <- pvalS + ((pvalB - pvalS) * (x[small] - lvalS))/(lvalB -
            lvalS)
    }
    else {
        val[small] <- pvalS
    }
    # this is the estimate of F at u:
    pu <- pp[lsmallpts - 1] + ((pp[lsmallpts] - pp[lsmallpts - 1]) * (u - obj$
        data[lsmallpts - 1]))/(obj$data[lsmallpts] - obj$data[lsmallpts - 1])
    #Nu <- lsmallpts
    valsm <- 1 - (1 - pu) * (1 + (k * (x[!small] - u))/obj$upper.par.ests["lambda"])^
        (-1/k)
    valsm[((k * (x[!small] - u))/obj$upper.par.ests["lambda"]) <= -1] <- 1
    val[!small] <- valsm
    val.orig <- val
    val.orig[sort.list(x.orig)] <- val
    val.orig
}
gpd.1q <- function(p, obj, linear = T)
{
    x.orig <- p
    p <- sort(p)
    if(oldClass(obj) != "gpd") {
        stop("Wrong object. Object has to be of class gpd")
    }
    n <- length(p)
    val <- vector(length = n, mode = "numeric")
    goodp <- p >= 0 & p <= 1
    val[!goodp] <- NA
    k <- obj$upper.par.ests["xi"]
    if(!SHAPE.XI) {
        k <-  - k
    }
    ndata <- obj$n
    u <- obj$upper.thresh
    pp <- (ppoints(obj$data))
    lsmallpts <- as.integer(sum(obj$data <= u) + 1)
    pu <- pp[lsmallpts - 1] + ((pp[lsmallpts] - pp[lsmallpts - 1]) * (u - obj$
        data[lsmallpts - 1]))/(obj$data[lsmallpts] - obj$data[lsmallpts - 1])
    small <- (p < pu) & goodp
    psm <- as.double(p[small])
    nsm <- as.integer(length(psm))
    smallpts <- as.double(pp[1:lsmallpts])
    oldind <- vector(length = nsm, mode = "numeric")
    oldind[1:nsm] <- -1
    lowInd <- .C("empirfunc",
        psm,
        smallpts,
        nsm,
        lsmallpts,
        as.integer(oldind))[[5]]
    highInd <- lowInd + 1
    lowInd[lowInd <= 0] <- NA
    lowP <- pp[lowInd]
    lowP[is.na(lowP)] <- 0
    highP <- pp[highInd]
    highVal <- obj$data[highInd]
    lowVal <- obj$data[lowInd]
    lowVal[is.na(lowVal)] <- 0
    if(linear) {
        val[small] <- lowVal + ((highVal - lowVal) * (psm - lowP))/(highP -
            lowP)
    }
    else {
        val[small] <- lowVal
    }
    quant <- u + (obj$upper.par.ests["lambda"] * (((1 - p[!small & goodp])/(1 - pu))^
        ( - k) - 1))/k
    val[(!small & goodp)] <- quant
    val.orig <- val
    val.orig[sort.list(x.orig)] <- val
    val.orig
}

gpd.2p <- function(x, est.object, linear = T)
{
    if(is(x, "timeSeries")) {
        x <- seriesData(x)
    }
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    x.orig <- x
    x <- sort(x)
    if(oldClass(est.object) != "gpd") {
        stop("Wrong object. Object has to be of class gpd")
    }
    if(is.na(est.object$lower.thres) || is.na(est.object$lower.par.ests["xi"])) {
        stop("Object must have 2 tails estimated")
    }
    N <- length(x)
    val <- vector(length = N)
    n <- est.object$n
    meadX <- est.object$lower.thresh <= x & x <= est.object$upper.thresh
    pp <- ppoints(est.object$data)
    xsm <- as.double(x[meadX])
    nsm <- as.integer(length(xsm))
    mdata.first.ind <- sum(est.object$data < est.object$lower.thresh)
    mdata.last.ind <- sum(est.object$data <= est.object$upper.thresh) + 1
    meadpts <- as.double(est.object$data[mdata.first.ind:mdata.last.ind])
    lmeadpts <- as.integer(length(meadpts))
    oldind <- vector(length = nsm, mode = "numeric")
    oldind[1:nsm] <- -1
    indB <- .C("empirfunc",
        xsm,
        meadpts,
        nsm,
        lmeadpts,
        as.integer(oldind))[[5]] + mdata.first.ind
    lvalB <- est.object$data[indB]
    indL <- indB - 1
    indL[indL == 0] <- NA
    lvalS <- est.object$data[indL]
    lvalS[is.na(lvalS)] <- (x[meadX])[is.na(lvalS)]
    lvalB[is.na(lvalB)] <- 0
    pvalB <- pp[indB]
    pvalS <- pp[indL]
    #lvalS[is.na(lvalS)] <- 0
    #lvalB[is.na(lvalB)] <- 0
    pvalS[is.na(pvalS)] <- 0
    pvalB[is.na(pvalB)] <- 0
    if(linear) {
        val[meadX] <- pvalS + ((pvalB - pvalS) * (x[meadX] - lvalS))/(lvalB -
            lvalS)
    }
    else {
        val[meadX] <- pvalS
    }
    # this is the estimate of F at upper threshold:
    p.upper <- pp[mdata.last.ind - 1] + ((pp[mdata.last.ind] - pp[mdata.last.ind -
        1]) * (est.object$upper.thresh - est.object$data[mdata.last.ind - 1]))/
        (est.object$data[mdata.last.ind] - est.object$data[mdata.last.ind -
        1])
    p.lower <- pp[mdata.first.ind] + ((pp[mdata.first.ind + 1] - pp[mdata.first.ind
        ]) * (est.object$lower.thresh - est.object$data[mdata.first.ind]))/
        (est.object$data[mdata.first.ind + 1] - est.object$data[mdata.first.ind
        ])
    uper.tail.x <- x > est.object$upper.thresh
    lower.tail.x <- x < est.object$lower.thresh
    k <- est.object$upper.par.ests["xi"]
    if(!SHAPE.XI) {
        k <-  - k
    }
    a <- est.object$upper.par.ests["lambda"]
    b <- est.object$upper.thresh
    val[uper.tail.x] <- 1 - (1 - p.upper) * (1 + (k * (x[uper.tail.x] - b))/a)^
        (-1/k)
    if(k < 0 & sum(x > b - a/k) > 0) {
        val[x > b - a/k] <- 1.
    }
    k <- est.object$lower.par.ests["xi"]
    if(!SHAPE.XI) {
        k <-  - k
    }
    a <- est.object$lower.par.ests["lambda"]
    b <- est.object$lower.thresh
    val[lower.tail.x] <- p.lower * (1 - (k * (x[lower.tail.x] - b))/a)^(-1/k)
    if(k < 0 & sum(x < b + a/k) > 0) {
        val[x < b + a/k] <- 0
    }
    val.orig <- val
    val.orig[sort.list(x.orig)] <- val
    val.orig
}

gpd.2q <- function(p, est.object, linear = T)
{
    x.orig <- p
    p <- sort(p)
    if(oldClass(est.object) != "gpd") {
        stop("Wrong object. Object has to be of class gpd")
    }
    if(is.na(est.object$lower.thres) || is.na(est.object$lower.par.ests["xi"])) {
        stop("Object must have 2 tails estimated")
    }
    N <- length(p)
    val <- vector(length = N, mode = "numeric")
    goodp <- p >= 0 & p <= 1
    val[!goodp] <- NA
    n <- est.object$n
    mdata.first.ind <- sum(est.object$data < est.object$lower.thresh)
    mdata.last.ind <- sum(est.object$data <= est.object$upper.thresh) + 1
    pp <- ppoints(est.object$data)
    p.upper <- pp[mdata.last.ind - 1] + ((pp[mdata.last.ind] - pp[mdata.last.ind -
        1]) * (est.object$upper.thresh - est.object$data[mdata.last.ind - 1]))/
        (est.object$data[mdata.last.ind] - est.object$data[mdata.last.ind -
        1])
    p.lower <- pp[mdata.first.ind] + ((pp[mdata.first.ind + 1] - pp[mdata.first.ind
        ]) * (est.object$lower.thresh - est.object$data[mdata.first.ind]))/
        (est.object$data[mdata.first.ind + 1] - est.object$data[mdata.first.ind
        ])
    meadX <- p.lower <= p & p <= p.upper
    xsm <- as.double(p[meadX])
    nsm <- as.integer(length(xsm))
if (nsm>0)
{
    meadpts <- as.double(pp[mdata.first.ind:mdata.last.ind])
    lmeadpts <- as.integer(length(meadpts))
    oldind <- vector(length = nsm, mode = "numeric")
    oldind[1:nsm] <- -1
    indB <- .C("empirfunc",
        xsm,
        meadpts,
        nsm,
        lmeadpts,
        as.integer(oldind))[[5]] + mdata.first.ind
    lvalB <- pp[indB]
    indL <- indB - 1
    indL[indL == 0] <- NA
    lvalS <- pp[indL]
    lvalS[is.na(lvalS)] <- 0
    lvalB[is.na(lvalB)] <- 0
    pvalB <- est.object$data[indB]
    pvalS <- est.object$data[indL]
    pvalS[is.na(pvalS)] <- 0
    pvalB[is.na(pvalB)] <- 0
    if(linear) {
        val[meadX] <- pvalS + ((pvalB - pvalS) * (p[meadX] - lvalS))/(lvalB -
            lvalS)
    }
    else {
        val[meadX] <- pvalS
    }
} 
    # this is the estimate of F at upper threshold:
    uper.tail.x <- p > p.upper & goodp
    lower.tail.x <- p < p.lower & goodp
    k <- est.object$upper.par.ests["xi"]
    if(!SHAPE.XI) {
        k <-  - k
    }
    a <- est.object$upper.par.ests["lambda"]
    b <- est.object$upper.thresh
    val[uper.tail.x] <- b + (a * (((1 - p[uper.tail.x])/(1 - p.upper))^( - k) -
        1))/k
    k <- est.object$lower.par.ests["xi"]
    if(!SHAPE.XI) {
        k <-  - k
    }
    a <- est.object$lower.par.ests["lambda"]
    b <- est.object$lower.thresh
    val[lower.tail.x] <- b - (a * (((p[lower.tail.x])/(p.lower))^( - k) - 1))/
        k
    val.orig <- val
    val.orig[sort.list(x.orig)] <- val
    val.orig
}

gpd.lmom <- function(lmom, location = NA, sample = NA)
{ 
        paramest <- c(NA,NA,NA)
        names(paramest) <- c("m","lambda","k")
        if (SHAPE.XI)  names(paramest) <- c("m","lambda","xi")

        if (!is.na(location)) 
        {
            if (length(lmom) > 4)
            {
                sample <- lmom
                lmom <- sample.LMOM(sample)
            }
        k <- lmom[1]/lmom[2] -2
        lambda <- (1+k) * lmom[1]
        m <- location
        }
        else 
        {
           if (length(lmom) > 4)
           {
               sample <- lmom
               lmom <- sample.LMOM(lmom)
           }
           if (is.na(sample[1]))
           {
               stop(paste("Problem in function gpd.lmom: either location parameter",
                    " or the sample observations should be specified"))
           }
           xx <- min(sample)
           n <- length(sample)
           k <- (n*(lmom[1] - xx) - 2*(n - 1) * lmom[2])/
                   ((n - 1)*lmom[2] - (lmom[1] - xx))
           lambda <- (1 + k) *(2 + k) * lmom[2]
           m <- xx - lambda/(n + k)
        }
        paramest[3] <- k;
        paramest[2] <- lambda;
        paramest[1] <- m;     
       
        if (SHAPE.XI) 
        { 
            paramest[3] <- -paramest[3]
            names(paramest) <-  c("m","lambda","xi")
        }
        
        val <- list(param.est = paramest)       
        val
}

gpd.ml <- function(sample, location = NA, init.est = NA, epsilon = 1e-6) 
{
    if(is.data.frame(sample))
        sample <- as.matrix(sample)

    n <- length(sample)
    lmomest <- init.est   
    if (!is.na(location)) 
    {   
        tmpX <- sample-location
        tmpX <- tmpX[tmpX>0]
        tmpN <- length(tmpX)
        assign("tempX",tmpX, pos = 1)
        assign("tempN",tmpN, pos =1)
        if (is.na(lmomest[1]))
        {
            lmomest <- gpd.lmom(lmom=sample.LMOM(sample), location = location)$param.est
            if (SHAPE.XI)  lmomest[3] <- -lmomest[3]
        }
        x0 <- c(lmomest[2],lmomest[3])
        negative.log.likelihood <- function(theta)
        {
            k <- theta[2]
            lambda <- theta[1]
            xsc <- 1 - (k*(tempX))/lambda
            ll <- NA
            if (sum(xsc < 0) > 0 | lambda < 0)
                ll <- NA
            else
                ll <- -tempN*log(lambda) + (1/k - 1)*sum(log(xsc))
            -ll
        }

        fit <- nlm(negative.log.likelihood,x0, iterlim = 200, steptol = epsilon)
        converged <- F
        if(fit$code<4) converged <- T              
        if (!converged)
        {
            #LMOM estimate might be bad... Try moment etimate as the intial starting point...
            tempMean <- mean(tempX)
            CV <- (tempMean * tempMean)/var(tempX)
            x0[1] <- 0.5 * tempMean * (CV+ 1)
            x0[2] <- 0.5 *(CV - 1)
            fit <- nlm(negative.log.likelihood,x0, iterlim = 200)
            if (fit$code>3)
                warning("Maximum Likelihood Method for the GPD did not converge")              
        }
        paramest <- c(location,fit$estimate[1], fit$estimate[2])
        names(paramest) <- c("m","lambda","k")
        if (SHAPE.XI)  names(paramest) <- c("m","lambda","xi")
        if (SHAPE.XI)  paramest[3] <- -paramest[3]    
    }        
    else
    {           
        assign("tempX",sample,pos= 1)
        assign("tempN",length(sample), pos =1)
        if (is.na(lmomest[1]))
        {
              lmomest <- gpd.lmom(lmom=sample.LMOM(sample), sample = sample)$param.est
              if (SHAPE.XI)  lmomest[3] <- -lmomest[3]
        }
        negative.log.likelihood <- function(theta)
        {
            # I use ll <- -10^10 for the function "optim" does not
            # seem to like NA's or Inf
              k <- theta[3]
              m <- theta[1]
              lambda <- theta[2]
              xsc <- 1 - k*(tempX-m)/lambda
              # ll <- NA
              ll <- -10^10
              if (sum(xsc < 0) > 0)
              #      ll <- NA
                    ll <- -10^10
              else
                    ll <- -tempN*log(lambda) + (1/k - 1)*sum(log(xsc))
              -ll
        }

        fit <- optim(lmomest, negative.log.likelihood, method="L-BFGS-B", lower=c(-Inf,0,-Inf), upper=c(min(tempX),Inf,Inf))
        converged <- F
        if (fit$convergence==0) converged <- T
        if (!converged)
               warning("Maximum Likelihood Method for the GPD did not converge")

        paramest <- fit$par
        names(paramest) <- c("m","lambda","k")
        if (SHAPE.XI)  names(paramest) <- c("m","lambda","xi")
        if (SHAPE.XI)  paramest[3] <- -paramest[3]
    }
    val <- list(n=n, data=sample, param.est = paramest, converged = converged)
    return(val)
}


gpd.tail <- function(data, one.tail=F, upper = NA, lower = NA, upper.method = "ml", lower.method = "ml", plot = T, ...)
{
    # Called "gpd.tail" to avoid confusion with McNeil's function "gpd"
    # Was "pot.1tail.est' and "pot.2tails.est" in EVANESCE

    if(is.data.frame(data)) 
        data <- as.matrix(data)
    n <- length(data)
    if(is.na(upper)) 
    {
        sorted.data <- sort(data)
        if(n <= 150/0.15)
        {
            uu1 <- sorted.data[n - trunc(n * 0.15)]
            uu2 <- sorted.data[n - trunc(n * 0.15) - 1]
            upper <- (uu1 + uu2)/2
        }
        else upper <- sorted.data[n - 150]
    }
    if(is.na(lower)) 
    {
        sorted.data <- sort(data)
        if(n <= 150/0.15)
        {
            uu1 <- sorted.data[trunc(n * 0.15)]
            uu2 <- sorted.data[trunc(n * 0.15) + 1]
            lower <- (uu1 + uu2)/2
        }
        else lower <- sorted.data[150]
    }

    # Analysis of the upper tail!
    upper.exceedances <- data[data > upper]
    excess <- upper.exceedances - upper
    if(casefold(upper.method, upper = F) == "ml") 
    {
        gpd.est.res <- gpd.ml(sample = excess, location = 0)
        gpd.est <- gpd.est.res$param.est
        upper.converged <- gpd.est.res$converged
        if(!upper.converged)
            warning(" MLE method for GPD did not converge for the upper tail. \n",
                "You can try to set the option upper.method = \"lmom\" for upper tail")
    }
    else if(casefold(upper.method, upper = F) == "lmom") 
    {
        lmom <- sample.LMOM(excess)
        gpd.est <- gpd.lmom(lmom, sample = excess, location = 0)$param.est
        upper.converged <- NA
    }
    else
        stop(paste("Unknown method for the parameter estimation: ", method))

    upper.par.ests <- c(gpd.est["lambda"], gpd.est["xi"])
    n.upper.exceed <- length(excess)
    p.less.upper.thresh <- 1 - n.upper.exceed/n
    if(plot) 
    {
        par.orig <- par()
        par(mfrow = c(2, 1))
        if(one.tail) par(mfrow = c(1, 1))
        qq <- qgpd(ppoints(excess), xi = gpd.est["xi"])
        plot(qq, sort(excess), xlab = paste("GPD Quantiles, for xi = ", gpd.est["xi"]),
                ylab="Excess over threshold", ...)
        title("Upper Tail")
    }
    
    if (!one.tail) 
    {
        # Analysis of the lower tail!
        lower.exceedances <- data[data < lower]
        excess <- lower - lower.exceedances
        if(casefold(lower.method, upper = F) == "ml")
        {
            gpd.est.res <- gpd.ml(sample = excess,location = 0)
            gpd.est <- gpd.est.res$param.est
            lower.converged <- gpd.est.res$converged
            if(!lower.converged)
            warning(" MLE method for GPD did not converge for the lower tail. \n",
                "You can try to set the option lower.method = \"lmom\" for the lower tail")
        }
        else if(casefold(lower.method, upper = F) == "lmom")
        {
            gpd.est <- gpd.lmom(sample = excess, location = 0)$param.est
            lower.converged <- NA
        }
        else
            stop(paste("Unknown method for the parameter estimation: ", method))

        lower.par.ests <- c(gpd.est["lambda"], gpd.est["xi"])
        n.lower.exceed <- length(excess)
        p.larger.lower.thresh <- 1 - n.lower.exceed/n
        if(plot) 
        {
            qq <- qgpd(ppoints(excess), xi = gpd.est["xi"])
            plot(qq, sort(excess), xlab = paste("GPD Quantiles, for xi = ", gpd.est["xi"]),
                ylab = "Excess over threshold", ...)
            title("Lower Tail")
#            par(par.orig)
            par(new = F)
        }
    } 
    else 
    {
        lower.exceed <- NA
        lower <- NA
        p.larger.lower.thresh <- NA
        n.lower.exceed <- NA
        lower.method <- NA
        lower.par.ests <- NA
        lower.converged <- NA
    }
    
    upper.exceed <- upper.exceedances
    if (!one.tail) lower.exceed <- lower.exceedances

    if(!one.tail)
        out <- list(n = length(data), data = sort(data),
            upper.exceed = upper.exceed, lower.exceed
             = lower.exceed, upper.thresh = upper,
            lower.thresh = lower, p.less.upper.thresh
            = p.less.upper.thresh, 
            p.larger.lower.thresh = 
            p.larger.lower.thresh, n.upper.exceed = 
            n.upper.exceed, n.lower.exceed = 
            n.lower.exceed, upper.method = 
            upper.method, lower.method = lower.method,
            upper.par.ests = upper.par.ests, 
            lower.par.ests = lower.par.ests, 
            upper.par.ses = NA, lower.par.ses = NA,
            upper.varcov = NA, lower.varcov = NA,
            upper.info = NA, lower.info = NA, 
            upper.converged = upper.converged, 
            lower.converged = lower.converged) 
        else
            out <- list(n = length(data), data = sort(data),
            upper.exceed = upper.exceed, upper.thresh = upper,
            p.less.upper.thresh = p.less.upper.thresh, 
            n.upper.exceed = n.upper.exceed, 
            upper.method = upper.method, 
            upper.par.ests = upper.par.ests, 
            upper.par.ses = NA,
            upper.varcov = NA,
            upper.info = NA,  
            upper.converged = upper.converged, 
            lower.converged = lower.converged) 
        
        oldClass(out) <- "gpd"
    return(out)
}

pgev <- function(x, m=0, lambda = 1, xi = 0)
{   
    k <- xi
    if (!SHAPE.XI) k <- -xi

    if (k==0) val <- exp(-exp(-(x - m)/lambda))
    else val <- exp( - (1. + (k * (x - m))/lambda)^(-1./k))       
    val
}

pgpd <- function(x, m = 0, lambda = 1, xi = 0)
{   
    k <- xi
    if (!SHAPE.XI) k <- -xi

    if (k==0) {
        val <- 1. - exp(-(x - m)/lambda)
        val[x<m] <- 0
    }
    else { 
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
    val 
}

plot.gpd <- function(gpd.obj, tail="upper",  optlog = NA, extend = 1.5, labels = T, ...)
{
if (tail == "upper") {
    data <- as.numeric(gpd.obj$upper.exceed)
    threshold <- gpd.obj$upper.thres
    xi <- gpd.obj$upper.par.ests["xi"]
    lambda <- gpd.obj$upper.par.ests["lambda"]
}

## added here!
if (tail == "lower") {
    data <- -as.numeric(gpd.obj$lower.exceed)
    threshold <- -gpd.obj$lower.thres
    xi <- gpd.obj$lower.par.ests["xi"]
    lambda <- gpd.obj$lower.par.ests["lambda"]
}

    choices <- c("Excess Distribution", 
        "Tail of Underlying Distribution", 
        "Scatterplot of Residuals", "QQplot of Residuals")
    tmenu <- paste("plot:", choices)
    pick <- 1
    while(pick > 0) {
        pick <- menu(tmenu, title =
            "\nMake a plot selection (or 0 to exit):")
        if(pick >= 3) {
            excess <- data - threshold
            res <- logb(1 + (xi * excess)/lambda)/xi
        }
        if(pick == 3) {
            plot(res, ylab = "Residuals", xlab = "Ordering"
                )
            lines(lowess(1:length(res), res))
        }
        if(pick == 4)
            qplot(res)
        if(pick == 1 || pick == 2) {
            plotmin <- threshold
            if(extend <= 1)
                stop("extend must be > 1")
            plotmax <- max(data) * extend
            x <- qgpd(seq(from = 0.001, to = 0.999, length = 1000),
                xi, threshold, lambda)
            x <- pmin(x, plotmax)
            x <- pmax(x, plotmin)
            ypoints <- ppoints(sort(data))
            y <- pgpd(x, xi, threshold, lambda)
        }
        if(pick == 1) {
            type <- "eplot"
            if(!is.na(optlog))
                alog <- optlog
            else alog <- "x"
            if(alog == "xy")
                stop("Double log plot of Fu(x-u) does not make much sense"
                    )
            yylab <- "Fu(x-u)"
            shape <- xi
            scale <- lambda
            location <- threshold
        }
        if(pick == 2) {
            type <- "tail"
            if(!is.na(optlog))
                alog <- optlog
            else alog <- "xy"
            if (tail == "upper") prob <- gpd.obj$p.less.upper.thresh
            if (tail == "lower") prob <- gpd.obj$p.larger.lower.thresh

            ypoints <- (1 - prob) * (1 - ypoints)
            y <- (1 - prob) * (1 - y)
            yylab <- "1-F(x)"
            shape <- xi
            scale <- lambda * (1 - prob)^xi
            location <- threshold - (scale * ((1 - prob)^
                ( - xi) - 1))/xi
        }
        if(pick == 1 | pick == 2) {
            plot(sort(data), ypoints, xlim = range(plotmin,
                plotmax), ylim = range(ypoints, y,
                na.rm = T), xlab = "", ylab = "", log
                 = alog, axes = T, ...)
            lines(x[y >= 0], y[y >= 0])
            if(labels) {
                xxlab <- "x"
                if(alog == "x" | alog == "xy" | alog ==
                    "yx")
                    xxlab <- paste(xxlab,
                        "(on log scale)")
                if(alog == "xy" | alog == "yx" | alog ==
                    "y")
                    yylab <- paste(yylab,
                        "(on log scale)")
                title(xlab = xxlab, ylab = yylab)
            }
            details <- paste("threshold = ", format(signif(
                threshold, 3)), "   xi = ", format(
                signif(shape, 3)), "   scale = ",
                format(signif(scale, 3)),
                "   location = ", format(signif(
                location, 3)), sep = "")
            print(details)
            lastcurve <- list(lastfit = gpd.obj, type =
                type, dist = "gpd", plotmin = plotmin,
                plotmax = plotmax, alog = alog,
                location = as.numeric(location), shape
                 = as.numeric(shape), scale =
                as.numeric(scale))
            assign("lastcurve", lastcurve, pos = 1)
        }
    }
}

ploting.position.estimator.LMOM <- function(sample, gamma = -0.35, delta = 0 ) 
{
       x <- sort(sample)
       n <- length(x)

       pp.pos <- function(i,n, gamma, delta) {
           (i + gamma)/(n + delta)
        }
       
       pp <- pp.pos(c(1:n),n,gamma,delta)
       ell1 <- sum(pp*x)/n
       ell2 <- sum((2*pp-1)*x)/n
       ell3 <- sum((6*pp*pp - 6*pp+1)*x)/n
       ell4 <- sum((20*pp^3-30*pp*pp +12*pp-1)*x)/n
      
       tau3 <- ell3/ell2
       tau4 <- ell4/ell2

       val <- c(ell1,ell2,tau3,tau4)
       names(val) <- c("ell_1", "ell_2", "tau_3", "tau_4")       
       val
}

qgev <- function(p,  m=0, lambda = 1, xi = 0) 
{
    k <- xi
    if (!SHAPE.XI) k <- -xi

    if (sum(p < 0 | p > 1)>0) warning("Argument of qgev should be between 0 and 1")
    if (k==0) val <- m - lambda * log(-log(p))
    else val <- m - lambda/k * ( 1 - (-log(p))^(-k))
    val [p < 0 | p > 1] <- NA
    val
}

qgpd <- function(p, m = 0, lambda = 1, xi = 0) 
{
    k <- xi
    if (!SHAPE.XI) k <- -xi

    if (sum(p <= 0 | p >= 1)>0) 
        warning("Argument of qgpd should be between 0 and 1")
    if (k==0)  
        val <- m - lambda*log(1 - p)
    else 
        val <- m - lambda*( 1 - (1 - p)^(-k))/k
    val[p <= 0 | p >= 1] <- NA
    val   
}

rgev <- function(n ,  m=0, lambda = 1, xi = 0) 
{
   qgev(runif(n),m,lambda,xi)
}

rgpd <- function(n , m = 0, lambda = 1, xi = 0) 
{
   qgpd(runif(n),m,lambda,xi)
}

sample.LMOM <- function(y) 
{
          x <- sort(y)
          N <- length(x)
          i <- c(1:N)

    
          fn1 <- N - i
          fn2 <- (N - i - 1)*fn1
          fn3 <- (N - i - 2)*fn2
          
          a1 <- sum(fn1/(N-1) * x)/N
          a2 <- sum(fn2/(N-1)/(N-2) * x)/N
          a3 <- sum(fn3/(N-1)/(N-2)/(N-3) * x)/N

          l1 <- mean(x)
          l2 <- l1 - 2*a1
          tau2 <- (l1 - 6.0*a1 + 6.0*a2)/l2
          tau3 <- (l1 - 12.0*a1 + 30.0*a2 - 20.0*a3)/l2   

          val <- c(l1,l2,tau2,tau3)
          names(val) <- c("ell_1", "ell_2", "tau_3", "tau_4")        
          
          val
}

shape.plot <- function(data, tail="upper", method = "ml", from = 0.5, to = 0.98, nint = 30)
{
if (tail == "upper") {
    thresh <- "Threshold"
    toptext <- "Percent Data Points above Threshold"
}
if (tail == "lower") {
    data <- -data
    thresh <- "Threshold"
    toptext <- "Percent Data Points below Threshold"
}
    if(is(data, "series")) {
        data <- seriesData(data)
    }
    if(is.data.frame(data)) {
        data <- as.matrix(data)
    }
    data <- sort(unclass(data))
    assign("tempData", data, pos = 1)
    estFun <- get(paste("gpd", method, sep = "."))
    assign("tempEstFun", estFun, pos = 1)

    n <- length(data)
    l1 <- data[trunc(from * n)]
    l2 <- data[trunc(to * n)]
    x <- pretty(c(l1, l2), n = nint)

    one.y <- function(u)
    {
        xx <- tempData[tempData > u]
        excess <- xx - u
        gpd.est <- tempEstFun(sample = excess, location = 0)$param.est
        c(gpd.est[3], length(xx)/length(tempData))
    }

    iii <- apply(as.matrix(x), 1, one.y)
    yy <- iii[1,  ]
    ylim <- range(yy)
    ylim[1] <- ylim[1] - 0.5
    ylim[2] <- ylim[2] + 0.5

    t1 <- "Estimate of k"
    if(SHAPE.XI)
        t1 <- "Estimate of xi"

if (tail == "lower")
{ 
    plot(-x, yy, type = "l", xlab = thresh, ylab = t1,
        ylim = ylim)
    nl <- length(pretty(x))
    xx <- pretty(x)
    indB <- .C("empirfunc",
        as.double(xx),
        as.double(x),
        as.integer(length(xx)),
        as.integer(length(x)),
        as.integer(1:length(xx)))[[5]] + 1
        axis(3, at = -x[indB], lab = paste(format(round(iii[2, indB] * 100))))
    mtext(toptext, side = 3, line = 3)
}
else
{
    plot(x, yy, type =

     "l", xlab = thresh, ylab = t1,
        ylim = ylim)
    nl <- length(pretty(x))
    xx <- pretty(x)
    indB <- .C("empirfunc",
        as.double(xx),
        as.double(x),
        as.integer(length(xx)),
        as.integer(length(x)),
        as.integer(1:length(xx)))[[5]] + 1
        axis(3, at = x[indB], lab = paste(format(round(iii[2, indB] * 100))))
    mtext(toptext, side = 3, line = 3)
}
}


tailplot <- function(gpd.obj, tail="upper", optlog = NA, extend = 1.5, labels = T, ...)
{
    if (tail == "upper")
    {
        data <- as.numeric(gpd.obj$upper.exceed)
        threshold <- gpd.obj$upper.thresh
        xi <- gpd.obj$upper.par.ests["xi"]
        lambda <- gpd.obj$upper.par.ests["lambda"]
    }
    if (tail == "lower")
    {
        data <- -as.numeric(gpd.obj$lower.exceed)
        threshold <- -gpd.obj$lower.thresh
        xi <- gpd.obj$lower.par.ests["xi"]
        lambda <- gpd.obj$lower.par.ests["lambda"]
    }
    plotmin <- threshold
    if(extend <= 1)
        stop("extend must be > 1")
    plotmax <- max(data) * extend
    x <- qgpd(seq(from = 0.001, to = 0.999, length = 999),threshold, lambda,xi)
    x <- pmin(x, plotmax)
    x <- pmax(x, plotmin)
    ypoints <- ppoints(sort(data))
    y <- pgpd(x,threshold, lambda,xi)
    type <- "tail"
    if(!is.na(optlog))
        alog <- optlog
    else alog <- "xy"
    if (tail == "upper") prob <- gpd.obj$p.less.upper.thresh
    if (tail == "lower") prob <- gpd.obj$p.larger.lower.thresh

    ypoints <- (1 - prob) * (1 - ypoints)
    y <- (1 - prob) * (1 - y)
    shape <- xi
    scale <- lambda * (1 - prob)^xi
    location <- threshold - (scale * ((1 - prob)^( - xi) -1))/xi
    plot(sort(data), ypoints, xlim = range(plotmin, plotmax), 
            ylim = range(ypoints, y, na.rm = T), xlab = "", ylab = "", log = alog, axes = T, ...)
    lines(x[y >= 0], y[y >= 0])
    if(labels) 
    {
        PlotType <- switch(alog,
             x = "log scale for x only",
             xy = "log - log scale",
             yx = "log - log scale",
             "natural scale"
             )
        xxlab <- switch(tail,
            upper = "x",
            lower = "-x"
        )
        yylab <- switch(tail,
            upper = "1-F(x)",
            lower = "F(x)"
        )
        title(main = paste("Plot of ",tail," tail in ", PlotType,sep=""),xlab = xxlab, ylab = yylab)
    }
    lastcurve <- list(lastfit = gpd.obj, type = type, dist
         = "gpd", plotmin = plotmin, plotmax = plotmax,
        alog = alog, location = as.numeric(location),
        shape = as.numeric(shape), scale = as.numeric(scale))
    assign("lastcurve", lastcurve, pos = 1)
    invisible()
}


qqexp <- function(x, nq = 50)
{
    values <- seq(from = 0.0001, to = 0.99,length = nq)
    qvalues <- qexp(values)
    plot(qvalues, quantile(x, probs=values),
    xlab="Theoretical Quantiles",
    ylab = "Sample Quantiles")
    title("Exponential Q-Q Plot")
    abline(lmrob(quantile(x, probs=values)~qvalues))
}
# depends upon "robustbase"
