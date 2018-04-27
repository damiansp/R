rm(list=ls())

op <- par(no.readonly=T)
# 2 AR(p) TS Process
# Simulation of AR(1) process with φ = 0.9
N <- 100
PHI <- 0.9
y <- arima.sim(n=N, list(ar=PHI), innov=rnorm(N))
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=T))
plot.ts(y, ylab='')
acf(y, main='Autocorrelations', ylab='', ylim=c(-1, 1), ci.col=2)
pacf(y, main='Partial Autocorrelations', ylab='', ylim=c(-1, 1), ci.col=2)