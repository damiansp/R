`gpd.ml` <-
function(sample, location = NA, init.est = NA, epsilon = 1e-6) 
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
            # I use ll <- -10^10 for the function "optim" does not
            # seem to like NA's or Inf
            k <- theta[2]
            lambda <- theta[1]
            xsc <- 1 - (k*(tempX))/lambda
			# ll <- NA
			ll <- -10^10
            if (sum(xsc < 0) > 0 | lambda < 0)
              # ll <- NA
              ll <- -10^10
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

