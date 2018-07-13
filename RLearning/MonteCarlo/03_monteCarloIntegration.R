#=========#=========#=========#=========#=========#=========#=========#=========
#==========================================#
#                                          #
#  Introducing Monte Carlo Methods with R  #
#  CHAPTER 3: Monte Carlo Integration	   #
#                                          #
#==========================================#
rm(list=ls())
setwd('~/Learning/R/RLearning/MonteCarlo')


# 1 Introduction
# Ex. 1 Compare results of the integrate() function to those of gamma()
ch <- function(la) {
  integrate(function(x) { x^(la - 1) * exp(-x) }, 0, Inf)$val
}

plot(lgamma(seq(0.01, 10, length=100)), 
	 log(apply(as.matrix(seq(0.01, 10, length=100)), 1, ch)), 
	 xlab='log(integrate(f))', 
	 ylab=expression(log(Gamma(lambda))))

# Ex. 2 From a sample of 10 Cauchy rvs, integrate() returns the wrong 
# numerical value
cac <- rcauchy(10) + 350
lik <- function(theta) {
  u <- dcauchy(cac[1] - theta)
  for (i in 2:10) {
    u <- u * dcauchy(cac[i] - theta)
  }
  u
}

integrate(lik, -Inf, Inf)
integrate(lik, 200, 400)

cac <- rcauchy(10)
nin <- function(a) {
  integrate(lik, -a, a)$val
}

nan <- function(a) {
  area(lik, -a, a)
}

x <- seq(1, 10^3, length = 10^4)
y <- log(apply(as.matrix(x), 1, nin))
z <- log(apply(as.matrix(x), 1, nan))
plot(x, y, type='l', ylim=range(cbind(y, z)))
lines(x, z, col=2)



# 2 Classic Monte Carlo Integration
# Ex. 3.3 Use toy function: h(x) = [cos(50x) + sin(20x)]^2;
# Eval integral over [0, 1]
h <- function(x) {
  ((cos(50 * x) + sin(20 * x)))^2
}

par(mar=c(2, 2, 2, 1), mfrow=c(2, 1))
curve(h, n=1000, xlab='Function', ylab = '')
integrate(h, 0, 1)

x <- h(runif(10^4))
estint <- cumsum(x) / (1:10^4)
esterr <- sqrt(cumsum((x - estint)^2)) / (1:10^4)
plot(estint, 
     xlab='Mean and Error Range', 
     type='l', 
     ylim=mean(x) + 20 * c(-esterr[10^4], esterr[10^4]), 
     ylab='')
lines(estint + 2 * esterr, col=2)
lines(estint - 2 * esterr, col=2)

# Ex. 4 Appoximating PHI(t) for ~N(0, 1) and sample size n
x <- rnorm(10^8)
bound <- qnorm(c(0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 0.999, 0.9999))
res <- matrix(0, ncol=8, nrow=7)
for (i in 2:8) {
  for (j in 1:8) {
    res[i - 1, j] <- mean(x[1:10^i] < bound[j])
  }
}

res <- matrix(as.numeric(format(res, digi=4)), ncol=8)
colnames(res) <- c('0.5', '0.75', '0.8', '0.9', '0.95', '0.99', '0.999', '0.999')
res
rm(x)




# 3 Importance Sampling
# 3.1 An arbitrary change of reference measure
# Ex. 3.5 Simulating probabilites in the far regions of tails
pnorm(4.5) # given the rarity of such an event, simulations will
           # only create them about 1x in 3 million...
# A corresponding importance sampler resticted to Z on [4.5, Inf)
nsim <- 10^4
Z <- 4.5
y <- rexp(nsim) + Z
weit <- dnorm(y) / dexp(y - Z)
plot(cumsum(weit) / 1:nsim, type='l')
abline(h=pnorm(-Z), col=2)
	
# Ex. 3.6 Simulating intractable gamma functions with conjugate priors
f <- function(a, b) {
  exp(2*(lgamma(a + b) - lgamma(a) - lgamma(b)) + a*log(0.3) + b*log(0.2))
}

aa <- 1:150
bb <- 1:100
post <- outer(aa, bb, f)
image(aa, bb, post, xlab=expression(alpha), ylab='')
contour(aa, bb, post, add=T)
	
# Simulate from t distribution
x <- matrix(rt(2 * 10^4, 3), ncol=2)
E <- matrix(c(220, 190, 190, 180), ncol=2) # Scale matrix
y <- t(t(chol(E)) %*% t(x) + c(50, 45))
points(y, cex=0.5, pch=16, col=rgb(0, 0, 0, 0.2))
	
ine <- apply(y, 1, min)
y <- y[ine > 0, ]
x <- x[ine > 0, ]
normx <- sqrt(x[, 1]^2 + x[, 2]^2)
f <- function(a) {
  exp(2 * (lgamma(a[, 1] + a[, 2]) - lgamma(a[, 1]) - lgamma(a[, 2])) 
	  + a[, 1] * log(0.3) + a[, 2] * log(0.2))
}
	
h <- function(a) {
  exp(1 * (lgamma(a[, 1] + a[, 2]) - lgamma(a[, 1]) - lgamma(a[, 2]))
      + a[, 1] * log(0.5) + a[, 2] * log(0.5) )
}
	
den <- dt(normx, 3)
mean(f(y) / den) / mean(h(y) / den)	# marginal likelihood
# Faulty code:
mean(y[, 1] * apply(y, 1, f) / den) / mean(apply(y, 1, h) / den)
# Assumed to be:
mean(y[, 1] * f(y) / den) / mean(h(y) / den)

	# 3.3.2 Sampling importance resampling
	# Ex. 3.7 (3.6 Continued)
	par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
	# Faulty code:
	weit <- (apply(y, 1, f) / den) / mean(apply(y, 1, h) / den)
	# Assumed to be:
	weit <- (f(y) / den) / mean(h(y) / den)
	image(aa, bb, post, xlab = expression(alpha), ylab = expression(beta))
	points( y[sample(1:length(weit), 10^3, replace = T, prob = weit), ], 
			cex = 0.5, pch = 16, col = rgb(0, 0, 0, 0.2) )
	boxplot(weit, ylab='importance weight')
	plot( cumsum(weit) / (1:length(weit)), type = 'l', xlab = 'simulations', 
		  ylab = 'marginal likelihood' )
	boot <- matrix(0, ncol = length(weit), nrow = 100)
	for (t in 1:100) {
		boot[t, ] <- cumsum(sample(weit)) / (1:length(weit))
	}
	uppa <- apply(boot, 2, quantile, 0.95)
	lowa <- apply(boot, 2, quantile, 0.05)
	polygon( c(1:length(weit), length(weit):1), c(uppa, rev(lowa)), 
			 col = 'gold' )
	lines(cumsum(weit) / (1:length(weit)), lwd = 2)	
	plot( cumsum(weit)^2 / cumsum(weit^2), type='l', xlab = 'simulations', 
		  ylab = 'Effective sample size' )
	
	# 3.3.3 Selection of Importance Function
	# Ex 3.8
	par(mfrow = c(1, 1)) 
	x <- rnorm(10^6)
	wein <- dcauchy(x) / dnorm(x)
	boxplot(wein / sum(wein))
	plot(cumsum(wein * (x > 2) * (x < 6)) / cumsum(wein), type = 'l')
	abline(h = pcauchy(6) - pcauchy(2), col = 'sienna')
	
#	mix <- function(n=1, p=0.5) {
#		m <- rbinom(1, size, prob=p)
#		c(simg(m), siml(n -m))
#	}

	sam1 <- rt(0.95 * 10^4, df=2)
	sam2 <- 1 + 0.5 * rt(0.05 * 10^4, df=2)^2
	sam <- sample(c(sam1, sam2), 0.95 * 10^4)
	weit <- dt(sam, df=2) / ( 0.95 * dt(sam, df=2) + 0.5 * (sam > 0) * 
							  dt(sqrt(2 * abs(sam - 1)), df=2) * sqrt(2) / 
							  sqrt(abs(sam - 1)) )
	# Oh look, more faulty code!
	plot(cumsum(h(sam1)) / (1:length(sam1)), type='l')
	




save.image('~/Desktop/R/MonteCarlo/MC.RData')
