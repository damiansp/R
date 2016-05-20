`gev.ml` <-
function(sample, init.est = NA, epsilon = 1e-5) 
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

