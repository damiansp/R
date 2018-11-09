#===================================#
#									#
#	Generalized Additive Models		#
#		An Introduction with R		#
#									#
#	Simon N. Wood. 2006				#
#									#
#                           #=======#
#							#
#	3. Intoducing GAMS		#
#							#
#===========================#

rm(list = ls())
setwd('~/Learning/R/RLearning/GAM')

library(gamair)
#library(lme4)
library(nlme)

data(engine)



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

# Est smoothing variance params
#For llm, see Ch. 2.4.2
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
tf.XD <- f






# 3.2 Univariate Smooth Functions

# Using the cubic spline basis
size <- c(1.42, 1.58, 1.78, 1.99, 1.99, 1.99, 2.13, 2.13, 2.13, 2.32, 2.32, 2.32, 
		  2.32, 2.32, 2.43, 2.43, 2.78, 2.98, 2.98)
wear <- c(4.0,  4.2,  2.5,  2.6,  2.8,  2.4,  3.2,  2.4,  2.6,  4.8,  2.9,  3.8,
		  3.0,  2.7,  3.1,  3.3,  3.0,  2.8,  1.7)
x <- size - min(size)
x <- x / max(x)
plot(x, wear, xlab = 'Scaled engine size', ylab = 'Wear index')

# Define R(x, z) for cubic spline on [0, 1]
rk <- function(x, z) {
  ((z - 0.5)^2 - 1/12) * ((x - 0.5)^2 - 1/12) / 4 -
    ((abs(x - z) - 0.5)^4 - (abs(x - z) - 0.5)^2 / 2 + 7/240) / 24
}

# Use as basis for a function that a sequence of knots and an x array to produce the 
# model matrix for the spline
spl.X <- function(x, xk) {
  # set up model matrix for cubic penalized regression spline
  q <- length(xk) + 2 	# n params
  n <- length(x)		# n data
  X <- matrix(1, n, q)	# init model matrix
  X[, 2] <- x
  X[, 3:q] <- outer(x, xk, FUN = rk)
  return (X)
}

# Set up knots
# Here a rank 6 basis is arbitrarily chosen (q = 6) -> 4 knots, evenly distributed on 
# [0, 1]:
xk <- 1:4 / 5
X <- spl.X(x, xk)
mod.1 <- lm(wear ~ X - 1)
xp <- 0:100 / 100	# x values to predict
Xp <- spl.X(xp, xk)	# prediction matrix
lines(xp, Xp %*% coef(mod.1), col = 2)	# fitted spline

  # 3.2.2 Controlling the degree of smoothing with penalized regression splines
  spl.S <- function(xk) {
  	# set up the penalized regression spline penalty matrix, given knots seqence xk
  	q <- length(xk) + 2
  	S <- matrix(0, q, q)	# init
  	S[3:q, 3:q] <- outer(xk, xk, FUN = rk)
  	return (S)
  }
  
  mat.sqrt <- function(S) {
  	# A simple matrix sqrt
  	d <- eigen(S, symmetric = T)
  	rS <- d$vectors %*% diag(sqrt(d$values)) %*% (d$vectors)
  }

  prs.fit <- function(y, x, xk, lambda) {
  	# function to fit penalized regression spline to x, y data, with knots xk and 
  	# smoothing param, lambda
  	q <- length(xk) + 2	# basis dims
  	n <- length(x)
  	
  	# Create augmented model matrix
  	Xa <- rbind(spl.X(x, xk), mat.sqrt(spl.S(xk)) * sqrt(lambda))
  	y[(n + 1):(n + q)] <- 0	# augment the data vector
  	lm(y ~ Xa - 1)	# fit and return penalized regression spline
  }

  # apply
  xk <- 1:7 / 8	# assign knots
  mod.2 <- prs.fit(wear, x, xk, 0.0001)	# fit pen. reg. spline
  Xp <- spl.X(xp, xk) # matrix to map params to fitted vals at xp
  lines(xp, Xp %*% coef(mod.2), col = 4)
  
  # 3.2.3 Choosing the smoothing parameter lambda via cross validation
  # with generalized cv
  lambda <- 1e-8
  n <- length(wear)
  V <- rep(0, 60)
  
  # loop through lambda vals
  for (i in 1:60) {
  	mod <- prs.fit(wear, x, xk, lambda) # fit mod given lambda
  	trA <- sum(influence(mod)$hat[1:n])	# find tr(A)
  	rss <- sum((wear - fitted(mod)[1:n])^2)	# rss
  	V[i] <- n * rss / (n -trA)^2			# gcv score
  	lambda <- lambda * 1.5					# increment
  }
  
  # plots csv scores by lambda
  plot(1:60, V, type = 'l', main = 'GSV score', xlab = i)
  
  # Fit the optimal model
  i <- (1:60)[V == min(V)]	# 39 = best i
  lambda.opt = 1.5^(i - 1) * 1e-8	# 0.049
  mod.3 <- prs.fit(wear, x, xk, lambda.opt)
  Xp <- spl.X(xp, xk)
  plot(x, wear)
  lines(xp, Xp %*% coef(mod.3), col = 2)


  # 3.3 Additive Models
  # Set up a two-term additive model
  am.setup <- function(x, z, q = 10) {
  	# Get X, S1 and S2
  	# Choose knots
  	xk <- quantile(unique(x), 1:(q - 2) / (q - 1))
  	zk <- quantile(unique(z), 1:(q - 2) / (q - 1))
  	
  	# Get penalty matrix
  	S <- list()
  	S[[1]] <- S[[2]] <- matrix(0, 2 * q - 1, 2 * q - 1)
  	S[[1]][2:q, 2:q] <- spl.S(xk)[-1, -1]
  	S[[2]][(q + 1):(2 * q - 1), (q + 1):(2 * q - 1)]  <- spl.S(zk)[-1, -1]
  	
  	# Get model matrix
  	n <- length(x)
  	X <- matrix(1, n, 2 * q - 1)
  	X[, 2:q] <- spl.X(x, xk)[, -1]	# first smooth
  	X[, (q + 1):(2 * q - 1)] <- spl.X(z, zk)[, -1]	# 2nd smooth
  	list(X = X, S = S)
  }
  
  # fit a 2-term additive model
  fit.am <- function(y, X, S, sp) {
  	# Get sqrt of total penalty matrix
  	rS <- mat.sqrt(sp[1] * S[[1]] + sp[2] * S[[2]])
  	q.tot <- ncol(X)	# n params
  	n <- nrow(X)		# n data
  	X1 <- rbind(X, rS)	# augmented X
  	y1 <- c(y, rep(0, q.tot))	# augmented data
  	b <- lm(y1 ~ X1 - 1)		# fit model
  	trA <- sum(influence(b)$hat[1:n])	# tr(A)
  	norm <- sum((y - fitted(b)[1:n])^2)	# RSS
  	list(model = b, gcv = norm * n / (n - trA)^2, sp = sp)
  }
	
  data(trees)
  rg <- range(trees$Girth)
  trees$Girth <- (trees$Girth - rg[1]) / (rg[2] - rg[1])	# scale data
  rh <- range(trees$Height)
  trees$Height <- (trees$Height - rh[1]) / (rh[2] - rh[1])
  
  am0 <- am.setup(trees$Girth, trees$Height)
  
  # Set up grid search to (appx) minimize gcv
  sp <- c(0, 0)	# init smoothing params
  # loop through sp grid
  for (i in 1:30) {
  	for (j in 1:30) {
      sp[1] <- 1e-5 * 2^(i - 1)
  	  sp[2] <- 1e-5 * 2^(j - 1)
  	  b <- fit.am(trees$Volume, am0$X, am0$S, sp)	# fit using current sps
  	  if (i + j == 2) {
  	  	best <- b	# initialize best with first mod
  	  } else if (b$gcv < best$gcv) {
		best <- b
  	  }
    }
  }

  # GCV best smoothing params found
  best$sp
  
  # Plot fitted vs data
  plot(trees$Volume, fitted(best$model)[1:31], xlab = 'Fitted Vol', 
  	   ylab = 'Actual Vol')
  
  # Eval and plot f[1] vs girth
  b <- best$model
  b$coeff[1] <- 0	# 0 intercept term
  b$coeff[11:19] <- 0	# 0 second smooth coeffs
  f0 <- predict(b)
  plot(trees$Girth, f0[1:31], xlab = 'Scaled Girth', ylab = expression(hat(f[1])))

  # Eval and plot f[2] vs girth
  b <- best$model
  b$coeff[2:10] <- 0	# 0 second smooth coeffs
  f0 <- predict(b)
  plot(trees$Height, f0[1:31], xlab = 'Scaled Height', 
  	   ylab = expression(hat(f[1])))



# 3.4 Generalized Additive Models
# Fit simple 2-term gam
# Gamma errors, log link
fit.gamG <- function(y, X, S, sp, threshold = 1e-4) {
  # Get sqrt of combined penalty matrix
  rS <- mat.sqrt(sp[1] * S[[1]] + sp[2] * S[[2]])
  q <- ncol(X)	# n params
  n <- nrow(X)	# n data
  X1 <- rbind(X, rS)	# aug. mod. matrix
  eta <- log(y)			# log-linked linear predictor
  norm <- 0
  old.norm <- 1			# init convergence controls
  
  # repeat convergence to within tolerance threshold
  while (abs(norm - old.norm) > threshold * norm) { 
  	mu <- exp(eta)				# fitted vals
  	z <- (y - mu) / mu + eta	# pseudodata (w[i] = 1 here)
  	z[(n + 1):(n + q)] <- 0		# augmented pseudodata
  	m <- lm(z ~ X1 - 1)			# fit penalized working mod
  	b <- coef(m)				# current param estimates
  	eta <- (X1 %*% b)[1:n]		# 'linear predictor'
  	trA <- sum(influence(m)$hat[1:n]^2)	# tr(A)
  	old.norm <- norm
  	norm <- sum((z - fitted(m))[1:n]^2)	# rss for working mod
  }
  
  list(model = m, gcv = norm * n / (n - trA)^2, sp = sp)
}

data(trees)
rg <- range(trees$Girth)
trees$Girth <- (trees$Girth - rg[1]) / (rg[2] - rg[1])	# scale data
rh <- range(trees$Height)
trees$Height <- (trees$Height - rh[1]) / (rh[2] - rh[1])
  
am0 <- am.setup(trees$Girth, trees$Height)
  
# Set up grid search to (appx) minimize gcv
sp <- c(0, 0)	# init smoothing params
# loop through sp grid
for (i in 1:30) {
  for (j in 1:30) {
    sp[1] <- 1e-5 * 2^(i - 1)
  	sp[2] <- 1e-5 * 2^(j - 1)
  	b <- fit.gamG(trees$Volume, am0$X, am0$S, sp)		# fit using current sps
  	if (i + j == 2) {
  	  best <- b	# initialize best with first mod
  	} else if (b$gcv < best$gcv) {
	  best <- b
  	}
  }
}

# GCV best smoothing params found
best$sp
  
# Plot fitted vs data
plot(trees$Volume, fitted(best$model)[1:31], xlab = 'Fitted Vol', 
  	 ylab = 'Actual Vol')
  
# Eval and plot f[1] vs girth
b <- best$model
b$coeff[1] <- 0	# 0 intercept term
b$coeff[11:19] <- 0	# 0 second smooth coeffs
f0 <- predict(b)
plot(trees$Girth, f0[1:31], xlab = 'Scaled Girth', ylab = expression(hat(f[1])))

# Eval and plot f[2] vs girth
b <- best$model
b$coeff[2:10] <- 0	# 0 second smooth coeffs
f0 <- predict(b)
plot(trees$Height, f0[1:31], xlab = 'Scaled Height', 
  	 ylab = expression(hat(f[1])))




save.image('~/Desktop/R/GAM/GAM.RData')