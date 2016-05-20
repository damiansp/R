`gpd.2q` <-
function(p, est.object, linear = T)
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

