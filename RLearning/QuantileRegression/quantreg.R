#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/QuantileRegression')

library(nor1mix)
library(quantreg)



# akj: univariate adaptive kernel density estimation
x <- c(rnorm(600), 2 + 2*rnorm(400))
xx <- seq(-5, 8, length=200)
z <- akj(x, xx)
plot(xx, z$dens, ylim=range(0, z$dens), type='l', col=2)
lines(density(x), col=4)
rug(x)

plot(xx, z$psi, type='l', main=expression('score ' * hat(psi) * "'" * (x)))

m3 <- norMix(mu=c(-4, 0, 3), sigma=c(1/3, 1, 2), w=c(0.1, 0.5, 0.4))
plot(m3, p.norm=F, ylim=c(0, 0.25))

x <- rnorMix(1000, m3)
z2 <- akj(x, xx)
lines(xx, z2$dens, col=2)
z3 <- akj(x, xx, kappa=0.5, alpha=0.88)
lines(xx, z3$dens, col=4)
rug(x)


# anova.rq