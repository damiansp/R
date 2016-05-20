`gpd.2p` <-
function(x, est.object, linear = T)
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

