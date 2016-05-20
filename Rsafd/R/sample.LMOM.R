`sample.LMOM` <-
function(y) 
{
          x <- sort(y)
          N <- length(x)
          i <- c(1:N)

    
          fn1 <- N - i
          fn2 <- (N - i - 1)*fn1
          fn3 <- (N - i - 2)*fn2
          
          a1 <- sum(fn1/(N-1) * x)/N
          a2 <- sum(fn2/(N-1)/(N-2) * x)/N
          a3 <- sum(fn3/(N-1)/(N-2)/(N-3) * x)/N

          l1 <- mean(x)
          l2 <- l1 - 2*a1
          tau2 <- (l1 - 6.0*a1 + 6.0*a2)/l2
          tau3 <- (l1 - 12.0*a1 + 30.0*a2 - 20.0*a3)/l2   

          val <- c(l1,l2,tau2,tau3)
          names(val) <- c("ell_1", "ell_2", "tau_3", "tau_4")        
          
          val
}

