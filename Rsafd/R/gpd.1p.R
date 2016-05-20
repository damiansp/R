`gpd.1p` <-
function(x, obj, linear = T)
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

