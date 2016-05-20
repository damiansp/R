`gpd.lmom` <-
function(lmom, location = NA, sample = NA)
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

