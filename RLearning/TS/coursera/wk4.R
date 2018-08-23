#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/TS/coursera')


# 1. PACF to Estimate the Order of AR(p) Processes
phi1 <- 0.9
phi2 <- -0.6
data.ts <- arima.sim(n=500, list(ar=c(phi1, phi2)))
par(mfrow=c(3, 1))
plot(data.ts, main=paste('AR(', phi1, ', ', phi2, ')', sep=''))
acf(data.ts)
#acf(data.ts, type='partial') # same as:
pacf(data.ts)

wheat <- read.csv('data/beveridge-wheat-price-index-1500.csv')
head(wheat)
wheat.ts <- ts(wheat[, 2], start=1500)
plot(wheat.ts, ylab='price', main='Beveridge Wheat Data')
wheat.ma <- filter(wheat.ts, rep(1/31, 31), sides=2)
lines(wheat.ma, col=2)

par(mfrow=c(3, 1))
y <- wheat.ts / wheat.ma
plot(y, ylab='scaled price', main='Wheat transformed')
acf(na.omit(y))
pacf(na.omit(y))

ar(na.omit(y), order.max=5) # infers coefs 0.72, -0.30
