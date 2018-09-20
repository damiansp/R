#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/TS/coursera')

library(isdals)
library(ppcor)
data(bodyfat)
data(LakeHuron)


# 1.1. PACF to Estimate the Order of AR(p) Processes
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


pairs(bodyfat[, c('Fat', 'Triceps', 'Thigh', 'Midarm')])
cor(bodyfat[, c('Fat', 'Triceps', 'Thigh', 'Midarm')])
fat.hat <- predict(lm(Fat ~ Thigh, bodyfat))
triceps.hat <- predict(lm(Triceps ~ Thigh, bodyfat))
cor((bodyfat$Fat - fat.hat), (bodyfat$Triceps - triceps.hat)) # partial correlation

# Can be gotten by:
pcor(bodyfat[, c('Fat', 'Triceps', 'Thigh')])
pcor(bodyfat[, c('Fat', 'Triceps', 'Thigh', 'Midarm')])

par(mfrow=c(3, 1))
plot(LakeHuron)
acf(LakeHuron)
pacf(LakeHuron)



# 2. Yule-Walker Equations (Matrix Notation) and Estimating AR(p) Parameters
sigma <- 4
phi <- c(1/3, 1/2)
n <- 1000

# Sim AR process
ar.process <- arima.sim(n, model=list(ar=phi), sd=sigma)
ar.process[1:5]
r <- acf(ar.process, plot=F)$acf[2:3]
r
R <- matrix(1, 2, 2)
R
R[1, 2] <- r[1]
R[2, 1] <- r[1]
b <- matrix(r, 2, 1)
b
phi.hat <- matrix(c(solve(R, b)[1, 1], solve(R, b)[2, 1]), 2, 1)
phi.hat

# Variance estimation
c0 <- acf(ar.process, type='covariance', plot=F)$acf[1]
var.hat <- c0 * (1 - sum(phi.hat * r))
var.hat

par(mfrow=c(3, 1))
plot(ar.process, main='Simulated AR(2)')
acf(ar.process, main='ACF')
pacf(ar.process, main='PACF')



# Estimation of model params in AR(3) sim
sigma <- 4
phi <- c(1/3, 1/2, 7/100)
n <- 100000

ar3.process <- arima.sim(n, model=list(ar=phi), sd=sigma)
r <- acf(ar3.process, plot=F)$acf[2:4]

R <- matrix(1, 3, 3)
R[1,2] <- r[1] 
R[1,3] <- r[2]
R[2,1] <- r[1]
R[2,3] <- r[1]
R[3,1] <- r[2]
R[3,2] <- r[1]
R

b <- matrix(nrow=3, ncol=1)
b[1,1] <- r[1]
b[2,1] <- r[2]
b[3,1] <- r[3]
b

phi.hat <- solve(R, b)
phi.hat

# sigma est
c0 <- acf(ar3.process, type='covariance', plot=F)$acf[1]
var.hat <- c0 * (1 - sum(phi.hat*r))
var.hat
(sigma.hat <- sqrt(var.hat))

par(mfrow=c(3, 1))
plot(ar3.process, main='Simulated AR(3)')
acf(ar3.process, main='ACF')
pacf(ar3.process, main='PACF')