# 2. Spurious Regression
rm(list=ls())
setwd('~/Learning/R/RLearning/TS/basic')

library(mvtnorm)
library(tseries)
data(USeconomic)

CBE <- read.table('./data/cbe.dat', header=T)
elec.ts <- ts(CBE[, 3], start=1958, freq=12)
choc.ts <- ts(CBE[, 1], start=1958, freq=12)
plot(as.vector(aggregate(choc.ts)), as.vector(aggregate(elec.ts)))
cor(aggregate(choc.ts), aggregate(elec.ts))

x <- rnorm(100)
y <- rnorm(100)
for (i in 2:100) {
  x[i] <- x[i - 1] + rnorm(1)
  y[i] <- y[i - 1] + rnorm(1)
}

par(mfrow=c(2, 1))
plot(x, type='l', ylim=range(c(x, y)))
lines(y, col=2)

plot(x, y)
abline(lm(y ~ x), col=2)
legend(
  'topleft', legend=paste('Corr:', round(cor(x, y), 3)), bg=rgb(1, 1, 1, 0.5))
  
x.rates <- read.table('./data/us_rates.dat', header=T)
head(x.rates)

acf(diff(x.rates$UK))
acf(diff(x.rates$EU))
plot(x.rates$EU / max(x.rates$EU), type='l')
lines(x.rates$UK / max(x.rates$UK), col=2)
plot(x.rates$UK, x.rates$EU)



# 3. Tests for Unit Root
# Augmented Dickey-Fuller test 
# Test with null hypothesis that a = 1 for 
# x[t] = ax[t - 1] + e[t]; where e is white noise; i.e. tests for unit root
adf.test(x)
# p = 0.5983: cannot reject hypothesis of a = 1

# Phillips-Perron Test: Same as adf, but estimates autocorrelations in e directly
pp.test(x.rates$UK) # p = 0.521
pp.test(x.rates$EU) # p = 0.7297
# Neither sig diff from unit root equation; e.g. ~ random walks



# 4 Cointegration
# 4.1 Definition
# Test for cointegration:
x <- y <- mu <- rep(0, 1000) # simulate cointegrated(x, y)
for (i in 2:1000) {
  mu[i] <- mu[i -1] + rnorm(1)
}
x <- mu + rnorm(1000)
y <- mu + rnorm(1000)
plot(mu, type='l', ylim=range(c(mu, x, y)))
lines(x, col=2)
lines(y, col=4)
adf.test(x) # p = 0.2939 (fits random walk)
adf.test(y) # p = 0.2731
# Phillips-Ouliaris test for cointigration 
po.test(cbind(x, y)) # p ≤ 0.01 (Null: not cointegrated; hence test suggest x, y 
                     #           are integrated)

# 4.2 Exchange Rate Series
po.test(cbind(x.rates$UK, x.rates$EU))
# p = 0.04, suggesting UK and EU are cointegrated

uk.eu.lm <- lm(UK ~ EU, data=x.rates)
par(mfrow=c(2, 2))
plot(uk.eu.lm)
summary(uk.eu.lm)

uk.eu.resid <- resid(uk.eu.lm)
uk.eu.resid.ar <- ar(uk.eu.resid)
uk.eu.resid.ar # order selected = 3; uk.eu.resid.ar$order
AIC(arima(uk.eu.resid, order=c(3, 0, 0))) # -9886.26
AIC(arima(uk.eu.resid, order=c(2, 0, 0))) # -9886.16
AIC(arima(uk.eu.resid, order=c(1, 0, 0))) # -9879.82
AIC(arima(uk.eu.resid, order=c(1, 1, 0))) # -9875.72
# AR(3) or AR(2) provide better fit that ARIMA(1, 1, 0), suggesting resids are 
# stationary process; supporting results of po test



# 5 Bivariate and Multivariate White Noise
cov.matrix <- matrix(c(1, 0.8, 0.8, 1), nrow=2)
w <- rmvnorm(1000, sigma=cov.matrix) # random sample from multivariate normal
cov(w)
head(w)
par(mfrow=c(1, 1))
plot(w)
wx <- w[, 1]
wy <- w[, 2]
ccf(wx, wy, main='') # cross correlation/covariance function
# cross correlations are ~0 for all non-0 lags.
par(mfrow=c(2, 1))
acf(wx)
acf(wy)



# 6. Vector Autoregressive Models
# Model:
# x[t] = theta[1, 1] * x[t - 1] + theta[1, 2] * y[t - 1] + w[x, t] and
# y[t] = theta[2, 1] * y[t - 1] + theta[2, 2] * y[t - 1] + w[y, t] or
# Z[t] = THETA*Z[t - 1] + w[t] where
# Z[t] = c(x[t], y[t]);
# THETA = [[theta[1,1], theta[1,2]]
#          [theta[2,1], theta[2,2]]
# w[t] = c(w[x, t], w[y, t])
# Stationary if roots of |THETA(x)| (det(THETA(x))) all > 1 in abs. val.
# for VAR(1) the determinant is
# | 1 - theta[1,1]x,    -theta[1,2]x |
# |   -theta[2, 1]x, 1 - theta[2,2]x | 
# = (1 - theta[1, 1]x)(1 - theta[2,2]x) - theta[1,2]theta[2,1]x^2
#
# Ex. THETA = [[0.4, 0.3],
#              [0.2, 0.1]]
# Det = | 1 - 0.4x,    -0.3x |
#       |    -0.2x, 1 - 0.1x | = 1 - 0.5x - 0.02x^2
Mod(polyroot(c(1, -0.5, -0.02))) # both have abs value > 1 so stationary

# Simulation
x <- y <- rep(0, 1000)
x[1] <- wx[1]
y[1] <- wy[1]
# Use the THETA matrix from above example:
for (i in 2:1000) {
  x[i] <- 0.4 * x[i - 1] + 0.3 * y[i - 1] + wx[i]
  y[i] <- 0.2 * x[i - 1] + 0.1 * y[i - 1] + wy[i]
}

plot(x, type='l', ylim=range(c(x, y)), col=2)
lines(y, col=4)

xy.ar <- ar(cbind(x, y))
xy.ar

# 6.1 VAR model fitted to US economic series
ts.plot(GNP, M1, col=c(1, 2))
legend('topleft', lty=1, col=1:2, legend=c('GNP', 'Real Money'))
us.ar <- ar(cbind(GNP, M1), method='ols', dmean=T, intercept=F)
us.ar # best model is VAR(3)
