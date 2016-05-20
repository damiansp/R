`ploting.position.estimator.LMOM` <-
function(sample, gamma = -0.35, delta = 0 ) 
{
       x <- sort(sample)
       n <- length(x)

       pp.pos <- function(i,n, gamma, delta) {
           (i + gamma)/(n + delta)
        }
       
       pp <- pp.pos(c(1:n),n,gamma,delta)
       ell1 <- sum(pp*x)/n
       ell2 <- sum((2*pp-1)*x)/n
       ell3 <- sum((6*pp*pp - 6*pp+1)*x)/n
       ell4 <- sum((20*pp^3-30*pp*pp +12*pp-1)*x)/n
      
       tau3 <- ell3/ell2
       tau4 <- ell4/ell2

       val <- c(ell1,ell2,tau3,tau4)
       names(val) <- c("ell_1", "ell_2", "tau_3", "tau_4")       
       val
}

