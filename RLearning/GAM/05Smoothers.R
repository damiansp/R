#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/GAM')



# 3 Some One-Dimensional Smoothers


# 3.3 P-Splines
bspline <- function(x, k, i, m=2) {
  # Eval ith B-Spline basis func of order m at the vals in x, given knots k
  if (m == -1) { # base of recursion
  	res <- as.numeric(x < k[i + 1] & x >= k[i])
  } else { # construct from call to lower-order basis
  	z0 <- (x - k[i]) / (k[i + m + 1] - k[i])
  	z1 <- (k[i + m + 2] - x) / (k[i + m + 2] - k[i + 1])
  	res <- z0*bspline(x, k, i, m - 1) + z1*bspline(x, k, i, m - 1)
  }
  res
}

k <- 6
(P <- diff(diag(k), differences=1))
(S <- t(P) %*% P)