#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
#detach('package:dplyr')
search()

op <- par(no.readonly=T)


# 2. AR(p) TS Process
# Simulation of AR(1) process with φ = 0.9
N <- 100
PHI <- 0.9
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=T))
y <- arima.sim(n=N, list(ar=PHI), innov=rnorm(N))
plot.ts(y, ylab='')
acf(y, main='Autocorrelations', ylab='', ylim=c(-1, 1), ci.col=2)
pacf(y, main='Partial Autocorrelations', ylab='', ylim=c(-1, 1), ci.col=2)



# 3. MA(q) Time Series Process
series <- rnorm(1000)
y.st <- filter(series, filter=c(0.6, -0.28), method='recursive')
par(mfrow=c(2, 1))
plot(series, type='l')
plot(y.st, type='l')
ar2.st <- arima(y.st, c(2, 0, 0), include.mean=F, transform.pars=F, method='ML')
ar2.st$coef
roots <- polyroot(c(1, -ar2.st$coef))
Mod(roots)
root.comp <- Im(roots)
root.real <- Re(roots)
x <- seq(-1, 1, length=1000)
y1 <- sqrt(1 - x^2)
y2 <- -y1
par(mfrow=c(1, 1))
plot(c(x, x), 
     c(y1, y2), 
     xlab='Real', 
     ylab='Complex', 
     type='l', 
     xlim=c(-2, 2), 
     ylim=c(-2, 2))
abline(h=0, col='darkgrey')
abline(v=0, col='darkgrey')
points(root.real, root.comp)
legend('topleft', legend='Roots of AR(2)', pch=1)



# 4. ARMA(p, q) Time Series Process
