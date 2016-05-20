`gpd.1q` <-
function(p, obj, linear = T)
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

