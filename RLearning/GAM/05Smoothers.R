#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/GAM')

library(mgcv)


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


# 3.6 SCOP-Splines (Shape Constrained P-Splines)
y <- seq(1, 10, length=100)
x <- -3*y + y^2 - 5*sin(0.05*y^3) + rnorm(length(y))
plot(x ~ y, type='l')
ssp <- s(x, bs='ps', k=k)
ssp$mono <- 1
sm <- smoothCon(ssp, data.frame(x))[[1]]
X <- sm$X
XX <- crossprod(X)
sp <- 0.5
gamma <- rep(0, k)
S <- sm$S[[1]]
for (i in 1:20) {
  gt <- c(gamma[1], exp(gamma[2:k]))
  dg <- c(1, gt[2:k])
  g <- -dg * (t(X) %*% (y - X %*% gt)) + sp * S %*% gamma
  H <- dg * t(dg * XX)
  gamma <- gamma - solve(H + sp*S, g)
}
gamma



# 4 So