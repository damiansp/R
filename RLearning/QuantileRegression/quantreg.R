#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/QuantileRegression')

library(nor1mix)
library(quantreg)
library(survival)
data(barro)
data(Bosco)
data(CobarOre)


# akj (pg. 3) Density Estimation using Adaptive Kernel Method
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


# anova.rq (pg. 5) ANOVA Function for Quantile Regression Fits
head(barro)
fit0 <- rq(y.net ~ lgdp2 + fse2 + gedy2, data=barro)
fit1 <- rq(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, data=barro)
fit2 <- rq(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, data=barro, tau=0.75)
fit3 <- rq(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, data=barro, tau=0.25)
anova(fit1, fit0)
anova(fit1, fit2, fit3)
anova(fit1, fit2, fit3, joint=F)
fit <- rq(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
          method='fn', 
          tau=1:4 / 5, 
          data=barro)
          
          
# bandwidth.rq (pg. 8)  Bandwidth Selection for rq Functions
# bandwidth.rq(p, n, hs=T, alpha=0.05)
# p: quantile(s) of interest     # n: sample size
# hs: hall-sheather method flag  # alpha: alpha for CI


# barro (pg. 9) Barro Data Set
# data(barro)


# boot.crq (pg. 9) Bootstrapping Censored Quantile Regression
# boot.crq(
#   x, y, c, taus, method, ctype='right', R=100, mboot, bmethod='jack', ...)
# x: model matrix
# y: response vec
# c: censor indicator
# taus: quantiles of interest
# method: 'P' (Portnoy), 'PH' (Peng & Huang)
# ctype: 'right', 'left'
# R: n bootstrap replicates
# mboot (optional): for bmethod='jack': number of drops; for 'xy-pair': size of 
#   samples
# bmethod: 'jack' (jackknife); 'xy-pair'; 'Bose'


# boot.rq (pg. 11) Bootstrapping Quantile Regression
y <- rnorm(50)
x <- matrix(rnorm(100), 50)
fit <- rq(y ~ x, tau=0.4)
summary(fit, se='boot', bsmethod='xy')
summary(fit, se='boot', bsmethod='pwy')
summary(fit, se='boot', bsmethod='mcmb')


# Bosco (pg. 13) Boscovich Data
head(Bosco)
plot(0:10 / 10, 0:10 * 100,
     xlab='sin^2(latitude)',
     ylab='arc-length of 1° latitude',
     type='n')
points(Bosco)
text(Bosco, pos=3, rownames(Bosco))
z <- rq(y ~ x, tau=-1, data=Bosco)
title('Boscovitch Ellipticity of the Earth')
xb <- c(0.85, 0.9, 0.6, 0.6)
yb <- c(400, 600, 450, 600)
for (i in 1:4) {
  abline(c(z$sol[4:5, i]))
  interval <- paste('t=(', format(round(z$sol[1, i], 2)), 
                    ', ', 
                    format(round(z$sol[1, i + 1], 2)), 
                    ')', 
                    sep='')
  text(xb[i], yb[i], interval)
}


# CobarOre (pg. 14) Cobar Ore Data
head(CobarOre)
plot(CobarOre)



# combos (pg. 15) Ordered Combinations
H <- combos(20, 3)


# critval (pg. 16) Hotelling Critical Values
# Critical values for uniform confidence bands for rqss fitting
# critval(kappa, alphp=0.05, rdf=0)
# kappa: length of tube
# alpha: alpha-level (CI = 1 - alpha)
# rdf: "residual" degrees of freedom of fitted object; Gaussian if 0, else t


# crq (pg. 17) Functions to fit censored quantile regression models
x <- sqrt(rnorm(100)^2) # abs(rnorm(100))
y <- -0.5 + x + (0.25 + 0.25*x) * rnorm(100)
s <- y > 0

plot(x, y, type='n')
points(x[s], y[s], pch=16, cex=0.9)
points(x[!s], y[!s], pch=1, cex=0.9)
y.latent <- y
y <- pmax(0, y)
yc <- rep(0, 100)
for (tau in (1:4) / 5) {
  f <- crq(Curv(y, yc) ~ x, tau=tau, method='Powell')
  xs <- sort(x)
  lines(xs, pmax(0, cbind(1, xs) %*% f$coef), col=2)
  abline(rq(y ~ x, tau=tau), col=4)
  abline(rq(y.latent ~ x), tau=tau, col=3)
}
legend('bottomright',
       legend=c('Naive QR', 'Censored QR', 'Omniscient QR'), 
       lty=1, 
       col=c(4, 2, 3))
       
# Example with left censoring
n <- 200
x <- rnorm(n)
y <- 5 + x + rnorm(n)

plot(x, y, cex = 0.5)
c <- 4 + x + rnorm(n)
d <- y > c
points(x[!d], y[!d], cex=0.5, col=2)

f <- crq(Surv(pmax(y, c), d, type='left') ~ x, method='Portnoy')
(g <- summary(f))
for (i in 1:4) {
  abline(coef(g[[i]])[, 1])
}


# dither (pg. 21) Randomly perturb a vector


