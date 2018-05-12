# 2. Spurious Regression
rm(list=ls())
setwd('~/Learning/R/RLearning/TS/basic')

library(tseries)


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
