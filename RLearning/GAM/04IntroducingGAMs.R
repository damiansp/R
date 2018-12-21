#===================================#
#									#
#	Generalized Additive Models		#
#		An Introduction with R		#
#									#
#	Simon N. Wood. 2006				#
#									#
#                           #=======#
#							#
#	4. Intoducing GAMS		#
#							#
#===========================#

rm(list = ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/GAM')

library(gamair)
library(lme4)
library(mgcv)
library(nlme)

data(engine)
data(trees)


# 2 Univariate Smoothing
# Using a piecewise linear basis
head(engine)
plot(engine$size, engine$wear, xlab='Engine capacity', ylab='Wear index')

# Generate jth tent function from set of knots xj
tent.basis <- function(x, xj, j) {
  dj <- numeric(length(xj))
  dj[j] <- 1
  approx(xj, dj, x)$y
}

# Basis matrix for data x and knot-sequence xj
tent.X <- function(x, xj) {
  n <- length(x)
  n.knots <- length(xj)
  X <- matrix(NA, n, n.knots)
  for (j in 1:n.knots) X[, j] <- tent.basis(x, xj, j)
  X
}

# Create 6 knots evenly srpread over x-range
knots <- seq(min(engine$size), max(engine$size), length=6)
X <- tent.X(engine$size, knots)
b <- lm(wear ~ X - 1, data=engine)
size.sim <- seq(min(engine$size), max(engine$size), length=200)
X.pred <- tent.X(size.sim, knots)
lines(size.sim, X.pred %*% coef(b), col=2)


# 2.2 Controlling smoothness by penalizing wigliness
# lambda = penalty parameter
penalized.regression.spline <- function(y, x, xj, lambda) {
  X <- tent.X(x, xj)
  D <- diff(diag(length(xj)), differences=2) # sqrt penalty
  X <- rbind(X, sqrt(lambda) * D)
  y <- c(y, rep(0, nrow(D)))
  lm(y ~ X - 1) # penalized least-squares fit
}

knots <- seq(min(engine$size), max(engine$size), length=20)
b <- penalized.regression.spline(engine$wear, engine$size, knots, lambda=2)
plot(engine$size, engine$wear)
X.pred <- tent.X(size.sim, knots)
lines(size.sim, X.pred %*% coef(b), col=2)

plot(engine$size, engine$wear)
lambdas <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128)
for (i in 1:length(lambdas)) {
  knots <- seq(min(engine$size), max(engine$size), length=20)
  b <- penalized.regression.spline(
    engine$wear, engine$size, knots, lambda=lambdas[i])
  X.pred <- tent.X(size.sim, knots)
  lines(size.sim, X.pred %*% coef(b), col=i)
}


# 2.3 Choosing the smoothing parameter, lambda, by cross-validation
lambdas <- seq(-9, 11, length=90)
n <- length(engine$wear)
V <- rep(NA, 90)
for (i in 1:90) {
  b <- penalized.regression.spline(
    engine$wear, engine$size, knots, exp(lambdas[i]))
  trF <- sum(influence(b)$hat[1:n])
  rss <- sum((engine$wear - fitted(b)[1:n])^2)
  V[i] <- n*rss / (n - trF)^2
}

plot(lambdas, V, type='l', xlab=expression(log(lambda)), main='GCV Score')
min(V) # 0.45
which(V == min(V)) # idx 54
lambdas[which(V == min(V))] # 2.9 -> exp(29)
best.lambda <- exp(lambdas[which(V == min(V))]) # 18.36
b <- penalized.regression.spline(engine$wear, engine$size, knots, best.lambda)
plot(engine$size, engine$wear, main='Optimal GCV Fit')
lines(size.sim, X.pred %*% coef(b), col=2)


# 2.4 The Bayesian/mixed-model alternative
X0 <- tent.X(size.sim, knots)                   # X in orig parameterization
D <- rbind(0, 0, diff(diag(20), difference=2)) 
diag(D) <- 1                                    # augmented D
X <- t(backsolve(t(D), t(X0)))                  # re-parameterize
Z <- X[, -c(1, 2)]                              # mixed model matrices
X <- X[, 1:2]                                   # """"""

# For llm, see Ch. 2.4.2
llm <- function(theta , X, Z, y) {
  # Untransform params
  sigma.b <- exp(theta[1])
  sigma <- exp(theta[2])
  
  # Extract dims
  n <- length(y)
  pr <- ncol(Z)
  pf <- ncol(X)
  
  # Obtain beta.hat, b.hat
  X1 <- cbind(X, Z)
  ipsi <- c(rep(0, pf), rep(1 / sigma.b^2, pr))
  b1 <- solve(crossprod(X1)/sigma^2 + diag(ipsi), t(X1) %*% y/sigma^2)
  
  # Compute log|Z'Z/sigma^2 + I/sigma.b^2|
  ldet <- sum(log(diag(chol(crossprod(Z)/sigma^2 + diag(ipsi[-(1:pf)])))))
  
  # Compute log profile likelihood
  l <- ((-sum((y - X1 %*% b1)^2)/sigma^2 
         - sum(b1^2*ipsi) 
         - n*log(sigma^2) 
         - pr*log(sigma.b^2) 
         - 2*ldet 
         - n*log(2*pi)) 
        / 2)
  attr(l, 'b') <- as.numeric(b1) # return beta.hat, b.hat
  -l
}

# ...something broken...
#m <- optim(c(0, 0), llm, method='BFGS', X=X, Z=Z, y=engine$wear)


# 3. Additive Models
# params: x = covariate vals; xk = knots
tent.XD <- function(x, xk, cmx=NULL, m=2) {
  # Get X and D subj to constraint
  nk <- length(xk)
  X <- tent.X(x, xk)[, -nk]                 # basis matrix
  D <- diff(diag(nk), differences=m)[, -nk] # root penalty
  if (is.null(cmx)) cmx <- colMeans(X)
  X <- sweep(X, 2, cmx)                     # subtract cmx from cols
  list(X=X, D=D, cmx=cmx)
}


# 3.2 Fitting Additive Models by Penalized Least Squares
am.fit <- function(y, x, v, sp, knots=10) {
  # sp = smoothing paramater (lambda)	
  # set up bases and penalty
  xk <- seq(min(x), max(x), length=knots)
  vk <- seq(min(v), max(v), length=knots)
  xdx <- tent.XD(x, xk)
  xdv <- tent.XD(v, vk)
  
  # Augmented model matrix and response
  nD <- nrow(xdx$D) * 2
  sp <- sqrt(sp)
  X <- cbind(c(rep(1, nrow(xdx$X)), rep(0, nD)),
             rbind(xdx$X, sp[1] * xdx$D, xdv$D * 0), 
             rbind(xdv$X, xdx$D * 0, sp[2] * xdv$D))
  y1 <- c(y, rep(0, nD))
  
  # Fit mod
  b <- lm(y1 ~ X - 1)
  n <- length(y)
  trA <- sum(influence(b)$hat[1:n]) # EDF
  rsd <- y - fitted(b)[1:n]         # resids
  rss <- sum(rsd^2)
  sig.hat <- rss / (n - trA)        # resid var
  gcv <- sig.hat*n / (n - trA)
  Vb <- vcov(b)*sig.hat / summary(b)$sigma^2 # coef cov matrix
  
  # Fitted mod
  list(b=coef(b), Vb=Vb, edf=trA, gcv=gcv, fitted=fitted(b)[1:n], rsd=rsd, 
       xk=list(xk, vk), cmx=list(xdx$cmx, xdv$cmx))
}


am.gcv <- function(least.squares.penalty, y, x, v, n.knots) {
  am.fit(y, x, v, exp(least.squares.penalty), n.knots)$gcv
}


# Find GCV optimal smoothing params
fit <- optim(
  c(0, 0), am.gcv, y=trees$Volume, x=trees$Girth, v=trees$Height, n.knots=10)
sp <- exp(fit$par)

# Get fit at opt smoothing param
fit <- am.fit(trees$Volume, trees$Girth, trees$Height, sp, knots=10)

# plot
am.plot <- function(fit, xlab, ylab) {
  start <- 2
  for (i in 1:2) {
    x <- seq(min(fit$xk[[i]]), max(fit$xk[[i]]), length=200)
    Xp <- tent.XD(x, fit$xk[[i]], fit$cmx[[i]])$X # pred matrix
    stop <- start + ncol(Xp) - 1
    ind <- start:stop
    b <- fit$b[ind]         # coefs
    Vb <- fit$Vb[ind, ind]  # cov matrix
    fv <- Xp %*% b          # smooths at x
    se <- rowSums((Xp %*% Vb) * Xp)^0.5 # se for smooths at x
    ul <- fv + 1.96*se
    ll <- fv - 1.96*se
    
    plot(x, fv, type='l', ylim=range(c(ul, ll)), xlab=xlab[i], ylab=ylab[i])
    lines(x, ul, col=2)
    lines(x, ll, col=2)
    start <- stop + 1
  }
}

par(mfrow=c(1, 3))
plot(fit$fitted, trees$Vol, xlab='fitted volume ', ylab='observed volume')
am.plot(fit, xlab=c('Girth', 'Height'), ylab=c('s(Girth)', 's(Height)'))



# 4. Generalized Additive Models
gam.fit <- function(y, x, v, sp, k=10) {
  # gamma error log-link 2 term gam fit
  eta <- log(y) # initial eta
  not.converged <- T
  old.gcv <- -100
  TOLERANCE <- 1e-5
  while (not.converged) {
  	mu <- exp(eta) # current est
  	z <- (y - mu) / mu + eta # pseudodata
  	fit <- am.fit(z, x, v, sp, k) # penalized least squares
  	if (abs(fit$gcv - old.gcv) < TOLERANCE * fit$gcv) not.converged <- F
  	old.gcv <- fit$gcv
  	eta <- fit$fitted
  }
  fit$fitted <- exp(fit$fitted)
  fit
}

gam.gcv <- function(lsp, y, x, v, k=10) { gam.fit(y, x, v, exp(lsp), k=k)$gcv }

fit <- optim(
  c(0, 0), gam.gcv, y=trees$Volume, x=trees$Girth, v=trees$Height, k=10)
sp <- exp(fit$par)
par(mfrow=c(1, 3))
plot(fit$fitted, trees$Volume, xlab='fitted volume', ylab='observed volume')



# 6. Introducing Package mgcv
ct1 <- gam(
  Volume ~ s(Height) + s(Height) + s(Girth), family=Gamma(link=log), data=trees)
ct1
par(mfrow=c(1, 2))
plot(ct1, residuals=T)


# 6.1 Finer Control of gam
# cr: cubic regression spline
ct2 <- gam(Volume ~ s(Height, bs='cr') + s(Girth, bs='cr'), 
           family=Gamma(link=log), 
           data=trees)
summary(ct2)
par(mfrow=c(1, 2))
plot(ct2, resid=T)

ct3 <- gam(Volume ~ s(Height) + s(Girth, bs='cr', k=20),
           family=Gamma(link=log),
           data=trees)
summary(ct3)
plot(ct3)

# minimize overfitting from GCV
ct4 <- gam(
  Volume ~ s(Height) + s(Girth), family=Gamma(link=log), data=trees, gamma=1.5)
summary(ct4)
plot(ct4)


# 6.2 Smooths of Several Variables
ct5 <- gam(Volume ~ s(Height, Girth, k=25), family=Gamma(link=log), data=trees)
summary(ct5)
plot(ct5, too.far=0.15)

ct6 <- gam(Volume ~ te(Height, Girth, k=5), family=Gamma(link=log), data=trees)
summary(ct6)
plot(ct6, too.far=0.15)


# 6.3 Parametric Model Terms
trees$Hclass <- factor(floor(trees$Height / 10) - 5, 
                       labels=c('small', 'medium', 'large'))
ct7 <- gam(Volume ~ Hclass + s(Girth), family=Gamma(link=log), data=trees)
summary(ct7)
par(mfrow=c(1, 2))
plot(ct7, all.terms=T)
anova(ct7)
AIC(ct7)