#=========#=========#=========#=========#=========#=========#=========#=========
# Online vignette: 
# https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
rm(list=ls())
setwd('~/Learning/R/RLearning/QuantileRegression')

library(datasets)
library(MASS)
library(nor1mix)
library(quantreg)
library(splines)
library(survival)
library(tripack)
library(zoo)
data(AirPassengers)
data(barro)
data(Bosco)
data(CobarOre)
data(engel)
data(Mammals)
data(mcycle)
data(stackloss)
data(UKDriverDeaths)


# Appendix A: Vignette
# A.3 Getting Started
#example(rq)
stack.loss # y
stack.x    # X = Air.Flow, Water.Temp, Acid.Conc
rq(stack.loss ~ stack.x, tau=0.5)  # median regression
rq(stack.loss ~ stack.x, tau=0.25) # 1st quartile
# Note that 8 of 21 points lie exactly on this plane in 4-space
rq(stack.loss ~ stack.x, tau=-1)   # full rq process
rq(rnorm(500) ~ 1, ci=False) # ordinary sample median 
rq(rnorm(500) ~ 1, weights=runif(500), ci=F) # weighted sample median

# plot of engel data and some rq lines
plot(engel$income, 
     engel$foodexp, 
     xlab='Household income', 
     ylab='Food expenditure', 
     type='n', 
     cex=0.5)
points(engel$income, engel$foodexp, cex=0.5, col=4)
taus <- c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95)
xx <- seq(min(engel$income), max(engel$income), 100)
f <- coef(rq((engel$foodexp) ~ (engel$income), tau=taus))
f <- coef(rq(engel$foodexp ~ engel$income, tau=taus))
yy <- cbind(1, xx) %*% f

for (i in 1:length(taus)) lines(xx, yy[, i], col='grey')
abline(lm(foodexp ~ income, data=engel), col=2, lty=2)
abline(rq(foodexp ~ income, data=engel), col=4)
legend('topleft',
       c("mean (LSE) fit", "median (LAE) fit"),
 	   col=c(2, 4),
 	   lty = c(2,1))

# Plotting coes and their confidence bands
plot(summary(rq(engel$foodexp ~ engel$income, tau=1:49/50)))

# Illustrate inequality constrained fitting
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
y <- 0.95 * apply(X, 1, sum) + rnorm(n)

# Constrain slope coefs to lie on [0, 1]
R <- cbind(0, rbind(diag(p), -diag(p)))
r <- c(rep(0, p), rep(-1, p))
rq(y ~ X, R=R, r=r, method='fnc')

fit1 <- rq(foodexp ~ income, tau=0.5, data=engel)
fit1
summary(fit1)
r1 <- resid(fit1)
plot(r1)
c1 <- coef(fit1)
summary(fit1, se='nid') # alt: se='ker' (Powell kernel); se='boot'


# A.5 Formal Inference
head(engel)
plot(engel$foodexp ~ engel$income, 
     col=4, 
     xlab='Household Income', 
     ylab='Food expenditure')
abline(rq(foodexp ~ income, tau=0.5, data=engel))
abline(lm(foodexp ~ income, engel), col=2)
taus <- c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95)
for (tau in taus) {
  abline(rq(foodexp ~ income, tau=tau, data=engel), col='grey')	
}

fit <- summary(rq(foodexp ~ income, tau=taus, data=engel))
summary(fit)

f1 <- rq(foodexp ~ income, tau=0.25, data=engel)
f2 <- rq(foodexp ~ income, tau=0.50, data=engel)
f3 <- rq(foodexp ~ income, tau=0.75, data=engel)

# Compare slopes
anova(f1, f2, f3)


# Save fit as table
pdf('engelcoef.ps', horizontal=F, width=6.5, height=3.5)
plot(fit, mfrow=c(1, 2))
dev.off()

#latex(fit, caption='Engel\'s Law', transpose=T)

# All distinct quantiles
z <- rq(foodexp ~ income, tau=-1, data=engel)

# Shorthand
engel$x <- engel$income
engel$y <- engel$foodexp
x.poor <- quantile(engel$x, 0.1)
x.rich <- quantile(engel$x, 0.9)

ps <- z$sol[1, ] # values of tau (quantiles)
qs.poor <- c(c(1, x.poor) %*% z$sol[4:5, ]) # 4, 5 are coefs (Intercept, income)
qs.rich <- c(c(1, x.rich) %*% z$sol[4:5, ])

par(mfrow=c(1, 2))
plot(c(ps, ps), 
     c(qs.poor, qs.rich), 
     type='n', 
     xlab=expression(tau), 
     ylab='quantile')
plot(stepfun(ps, c(qs.poor[1], qs.poor)), 
     do.points=F, 
     add=T, 
     col.hor=2, 
     col.vert=2)
plot(stepfun(ps, c(qs.poor[1], qs.rich)), 
     do.points=F, 
     add=T, 
     col.hor=4, 
     col.vert=4)

ps.wts <- (c(0, diff(ps)) + c(diff(ps), 0)) / 2
ap <- akj(qs.poor, z=qs.poor, p=ps.wts)
ar <- akj(qs.rich, z=qs.rich, p=ps.wts)
plot(c(qs.poor, qs.rich), 
     c(ap$dens, ar$dens), 
     type='n', 
     xlab='Food Expenditure', 
     ylab='Density')
lines(qs.rich, ar$dens, col=4)
lines(qs.poor, ap$dens, col=2)
legend('topright', c('Poorest 10%', 'Richest 10%'), lty=1, col=c(2, 4))



# A.7 Inference on the Quantile Regression Process
# Repeat with online example
source('gasprice.R')
head(gasprice)
plot(gasprice)
x <- gasprice
n <- length(x)
p <- 5 # lag length
X <- cbind(x[(p - 1):(n - 1)], 
           x[(p - 2):(n - 2)], 
           x[(p - 3):(n - 3)], 
           x[(p - 4):(n - 4)])
y <- x[p:n]
T1 <- KhmaladzeTest(y ~ X, taus=-1, nullH='location')
T2 <- KhmaladzeTest(y ~ X, taus=10:290 / 300, nullH='location', se='ker')



# A.8 Nonlinear Quantile Regression 
n <- 200
df <- 8
delta <- 8
x <- sort(rt(n, df))
u <- runif(n)
v <- (-log(1 - (1 - exp(-delta)) / (1 + exp(-delta*pt(x, df))*((1/u) - 1))) 
      / delta)
y <- qt(v, df)



# A.9 Nonparametric Quantile Regression
lpqr <- function(x, y, h, m=50, tau=0.5) {
  xx <- seq(min(x), max(x), length=m)
  fv <- xx
  dv <- xx
  for (i in 1:length(xx)) {
    z <- x - xx[i]
    wx <- dnorm(z / h)
    r <- rq(y ~ z, weights=wx, tau=tau, ci=F)
    fv[i] <- r$coef[1]
    dv[i] <- r$coef[2]
  }
  list(xx=xx, fv=fv, dv=dv)
}

plot(x, y, col=4, cex=0.25)
us <- c(0.25, 0.5, 0.75)
for (i in 1:length(us)) {
  u <- us[i]
  v <- (-log(1 - (1 - exp(-delta)) / (1 + exp(-delta*pt(x, df))*((1/u) - 1))) 
        / delta)
  lines(x, qt(v, df), col='grey')
}

dat <- NULL
dat$x <- x
dat$y <- y
deltas <- matrix(0, 3, length(us))

frank.mod <- function(x, delta, mu, sigma, df, tau) {
  z <- qt((-log(1 - (1 - exp(-delta)) 
           / (1 + exp(-delta*pt(x, df))*((1/tau) - 1))) / delta), 
          df)
  mu + sigma*z
}

for (i in 1:length(us)) {
  tau <- us[i]
  fit <- nlrq(y ~ frank.mod(x, delta, mu, sigma, df=8, tau=tau),
              data=dat,
              tau=tau,
              start=list(delta=5, mu=0, sigma=1), 
              trace=T)
  lines(x, predict(fit, newdata=x), col='#444444', lwd=2)
  deltas[i, ] <- coef(fit)
}


head(mcycle)
plot(mcycle$times, mcycle$accel, xlab='ms', ylab='acceleration')
hs <- 1:4
i <- 1
for (h in hs) {
  fit <- lprq(mcycle$times, mcycle$accel, h=h, tau=0.5)
  lines(fit$xx, fit$fv, col=i)
  i <- i + 1
}
legend('bottomright', legend=c('h=1', 'h=2', 'h=3', 'h=4'), lty=1, col=1:4)

plot(mcycle$times, mcycle$accel, xlab='ms', ylab='acceleration')
X <- model.matrix(mcycle$accel ~ bs(mcycle$times, df=15))
i <- 1
for(tau in 1:3 / 4) {
  fit <- rq(mcycle$accel ~ bs(mcycle$times, df=15), tau=tau)
  accel.fit <- X %*% fit$coef
  lines(mcycle$times, accel.fit, col=i)
  i <- i + 1
}


# Sample syntax with covariate added
fit <- rq(y ~ bs(x, df=5) + z, tau=0.33)

head(Mammals)
x <- log(Mammals$weight)
y <- log(Mammals$speed)
plot(x, y, xlab='Weight (log(kg))', ylab='Speed (log(km/h))', type='n')
points(x[Mammals$hoppers], y[Mammals$hoppers], pch='H', col=2)
points(x[Mammals$specials], y[Mammals$specials], pch='S', col=4)
others <- (!Mammals$hoppers & !Mammals$specials)
points(x[others], y[others], pch='O')

fit.upper <- rqss(y ~ qss(x, lambda=2), tau=0.9)
fit.lower <- rqss(y ~ qss(x, lambda=2), tau=0.1)
plot(fit.upper, add=T)
plot(fit.lower, add=T)


head(CobarOre)
fit <- rqss(z ~ qss(cbind(x, y), lambda=0.1, ndum=100), data=CobarOre)
plot(fit, axes=F, xlab='', ylab='')