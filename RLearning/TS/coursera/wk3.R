#=========#=========#=========#=========#=========#=========#=========#=========
setwd('~/Learning/R/RLearning/TS/coursera')

par(mfrow=c(3, 1))
plot(arima.sim(n=150, list(order=c(0, 0, 0))), main='White noise')
plot(arima.sim(n=150, list(ma=c(0.33, 0.33, 0.33))), main='MA(3)')
plot(arima.sim(n=150, list(ma=rep(0.2, 5))), main='MA(5)')