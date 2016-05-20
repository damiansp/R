`gev.lmom` <-
function(lmom)
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

