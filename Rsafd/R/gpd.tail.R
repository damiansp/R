`gpd.tail` <-
function(data, one.tail=F, upper = NA, lower = NA, upper.method = "ml", lower.method = "ml", plot = T, ...)
{
    # Called "gpd.tail" to avoid confusion with McNeil's function "gpd"
    # Was "pot.1tail.est' and "pot.2tails.est" in EVANESCE

    if(is.data.frame(data)) 
        data <- as.matrix(data)
     if(is.zoo(data)) 
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

