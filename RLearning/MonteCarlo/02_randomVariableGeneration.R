#=========#=========#=========#=========#=========#=========#=========#=========
#==========================================#
#                                          #
#  Introducing Monte Carlo Methods with R  #
#                                          #
#                                       #==#
#										#
# CHAPTER 2: Random Variable Generation #
#										#
#=======================================#
rm(list=ls())
setwd('~/Learning/R/RLearning/MonteCarlo')

library(mcsm)
library(splines)


# 1. Introduction
# 1.1 Uniform distribution
n.sim <- 10000
x <- runif(n.sim)
hist(x)
x1 <- x[-n.sim]
x2 <- x[-1]
plot(x1, x2)		
acf(x)
		
# 1.2 The inverse transform
U <- runif(n.sim)
X <- -log(U)
Y <- rexp(n.sim)
par(mfrow=c(1,2))
hist(X, freq=F, main='Exp from Uniform')
lines(density(X), col=4)
hist(Y, freq=F, main='Exp from rexp')
lines(density(Y), col=4)


		
# 2 General Transformation Methods
U <- runif(3000)
U <- matrix(U, nrow=3)
X <- -log(U)	 # Uniform to exponential
X <- 2 * apply(X, 2, sum) # Exponential to Chi Square w/ 3 df
par(mfrow=c(2, 1))
hist(U)
hist(X)

# 2.1 A Normal Generator
# 2.2 Discrete Distributions
n.sim <- 4000
lambda <- 100
spread <- 3 * sqrt(lambda) # ~3sd
tt <- round(seq(max(0, lambda - spread), lambda + spread, 1))
prob <- ppois(tt, lambda)
plot(prob ~ tt, type='l')
X <- numeric(n.sim)
for (i in 1:n.sim) {
  u <- runif(1)
  X[i] <- tt[1] + sum (prob < u)
}
hist(X)
		
# 2.3 Mixture Representations
# Ex: X ~ Neg(n, p) then: X|y ~ P(y) and Y ~ G(n, beta)
n.sim <- 1000
n <- 6
p <- 0.3
y <- rgamma(n.sim, n, rate=p / (1 - p))
x <- rpois(n.sim, y)
hist(x, main='', freq=F, col='cyan', border='blue', breaks=40)
lines(0:50, dnbinom(0:50, n, p), lwd=2, col='sienna')
		





save.image('~/Desktop/R/MonteCarlo/MC.RData')
