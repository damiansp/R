`estimate.mix.gev` <-
function(sample, init.est = NA, sampLmom = NA, epsilon = 1e-5) 
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

