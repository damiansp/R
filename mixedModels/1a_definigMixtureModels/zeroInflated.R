#---------#---------#---------#---------#---------#---------#---------#---------
setwd('~/Learning/R/mixedModels/1_mixturedOfGaussians')

# Zero-inflated counts
x <- 0:15
y <- dnbinom(x, 8, 0.6)
par(mfrow=c(2, 1))
barplot(y, names.arg=x, las=1, xlab='x', ylab='Prob. mass', ylim=c(0, 0.25))
z <- 0.2*c(1, rep(0, length(x) - 1)) + (1 -0.2)*y
barplot(z, names.arg=x, las=1, xlab='x', ylab='Prob. mass', ylim=c(0, 0.25))


# Zero-inflated continuous
x <- seq(-2, 15, length=200)
y <- plnorm(x, 1.5, 0.5) # log-normal
z <- 0.3*as.numeric(x >= 0) + (1 - 0.3)*y
par(mfrow=c(1, 1))
plot(x, y, type='l', las=1, xlab='x', ylab='Cum. distrib. function')
lines(x, z, col=2)
legend('topleft', 
       lty=1, 
       col=1:2, 
       legend=c('Log-Normal', 'Zero-Inflated Log-Normal'))