#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
#detach('package:dplyr')
search()

library(forecast)
library(urca)
data(npext)

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
head(npext)
y <- ts(na.omit(npext$unemploy), start=1909, end=1988, freq=1)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=T))
plot(y, ylab='log(unemployment rate)')
acf(y, main='Autocorrelations', ylab='', ylim=c(-1, 1))
pacf(y, main='Partial Autocorrelations', ylab='', ylim=c(-1, 1))
par(op)

# Tentative ARMA(2, 0)
arma20 <- arima(y, order=c(2, 0, 0))
ll20 <- logLik(arma20)
aic20 <- arma20$aic
res20 <- residuals(arma20)
# Test assumption of uncorrelatedness
Box.test(res20, lag=20, type='Ljung-Box')
shapiro.test(res20)

# alternate specifications
arma30 <- arima(y, order=c(3, 0, 0))
ll30 <- logLik(arma30)
aic30 <- arma30$aic # better
lrtest <- as.numeric(2*(ll30 - ll20))
chi.pval <- pchisq(lrtest, df=1, lower.tail=F) # but not signif better than 2, 0

arma11 <- arima(y, order=c(1, 0, 1))
ll11 <- logLik(arma11)
aic11 <- arma11$aic # better still
tsdiag(arma11)
res11 <- residuals(arma11)
Box.test(res11, lag=20, type='Ljung-Box')
shapiro.test(res11)

auto.arima(y, max.p=3, max.q=3, start.p=1, start.q=0, ic='aic')

# Forecast
arma11.pred <- predict(arma11, n.ahead=10)
pred <- ts(c(rep(NA, length(y) - 1), y[length(y)], arma11.pred$pred),
           start=1909,
           freq=1)
upper <- ts(c(rep(NA, length(y) - 1), 
              y[length(y)], arma11.pred$pred + 1.96*arma11.pred$se),
            start=1909,
            freq=1)
lower <- ts(c(rep(NA, length(y) - 1), 
              y[length(y)], arma11.pred$pred - 1.96*arma11.pred$se),
            start=1909,
            freq=1)
observed <- ts(c(y, rep(NA, 10)), start=1909, freq=1)
plot(observed, type='l', ylab='log(unemployment)', xlab='')
lines(pred, col=4)
lines(upper, col=2, lty=4)
lines(lower, col=2, lty=4)