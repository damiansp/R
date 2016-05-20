library(pspline)

n <- 100
x <- (1:n) / n
actual <- ((exp(1.2*x) + 1.5*sin(7*x)) - 1) / 3

par(mfrow=c(2,2))
noise <- rnorm(n, 0, 0.15)

y <- actual + noise
fit <- smooth.Pspline(x, y, method = 3)

#postscript('result.ps', height=4, width=5, horizo=F)

plot(x, y, xlab='x', ylab='y', cex=0.5)
lines(x, actual, lty=2)
lines(fit$x, fit$y)

#graphics.off()

#SP500 data:
par(mfrow=c(1,1))
x <- (1:(length(SP))) / length(SP)
fit <- smooth.Pspline(x, SP, method=3)
plot(x, SP, xlab='days', ylab='S&P')
lines(fit$x, fit$y, col=3)