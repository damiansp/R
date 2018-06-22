#==========================================#
#                                          #
#  Introducing Monte Carlo Methods with R  #
#                                          #
#                                  #=======#
#								   #
#  CHAPTER 1: Basic R Programming  #
#								   #
#==================================#

rm(list=ls())

library(mcsm)
library(splines)


Nit <- c(0,0,0, 1,1,1, 2,2,2, 3,3,3, 4,4,4, 6,6,6)
AOB <- c( 4.3, 4.2, 4.7, 6.1, 5.9, 6.9, 6.9, 6.3, 6.8, 6.3, 6.6, 6.5, 7.4, 7.4, 7.7, 
		  7.8, 8.1, 7.2 )
AOBm <- tapply(AOB, Nit, mean)
Nitm <- tapply(Nit, Nit, mean)
plot(Nit, AOB, xlim=c(0,6), ylim=c(min(AOB), max(AOB)))
fitAOB <- lm(AOBm ~ ns(Nitm, df=2))	# ns = natural spline
xmin <- min(Nit)
xmax <- max(Nit)
lines(seq(xmin, xmax, 0.1), predict(fitAOB, data.frame(Nitm=seq(xmin, xmax, 0.1))))
fitAOB2 <- loess(AOBm ~ Nitm, span=1.25)
lines(seq(xmin, xmax, 0.1), predict(fitAOB2, data.frame(Nitm=seq(xmin, xmax, 0.1))), 
	  col=2)
	  
x <- seq(-3, 3, le=5)
y <- 2 + 4*x + rnorm(5)

plot(y ~ x)
fit <- lm(y ~ x)
summary(fit)
abline(fit)

names(fit)

# to retrieve residual standard error:
rse <- function(mod) {
	sqrt(sum(mod$res^2) / mod$df)
}

rse(fit)

# Bootstrap the model by resampling from the residual errors
rData <- fit$residuals
nBoot <- 5000
b <- array(0, dim=c(nBoot, 2))
for (i in 1:nBoot) {
	yStar <- y + sample(rData, replace=T)
	bFit <- lm(yStar ~ x)
	b[i, ] <- bFit$coefficients
}

hist(b[, 1], main='Intercept')
hist(b[, 2], main='Slope')
#plot(y ~ x)
#for (i in 1:nBoot) {
#	abline(b[i, 1], b[i, 2], col=i)
#}



# 1.6 Graphical Facilities 

plot(faithful)
plot(faithful$eruptions ~ faithful$waiting)

jpeg(file = 'faith.jpg')	# opens a device to print jpg to file
par(mfrow = c(1, 2), mar = c(4, 2, 2, 1))
hist(faithful[, 1], nclass = 21, col = 'wheat', main = '', xlab = names(faithful)[1])
hist(faithful[, 2], nclass = 21, col = 'wheat', main = '', xlab = names(faithful)[2])
dev.off()

plot(as.vector(time(mdeaths)), as.vector(mdeaths), cex=0.6)
lines(spline(mdeaths), lwd=2, col='chocolate', lty=3)
ari <- arima(mdeaths, order=c(1, 0 , 0), seasonal=list(order=c(1, 0, 0), period=12))$coef
lines(as.vector(time(mdeaths))[-(1:13)], ari[3] + ari[1] * 
	  (mdeaths[-c(1:12, 72)] - ari[3]) + ari[2]*(mdeaths[-(60:72)] - ari[3]), 
	  lwd=2, col='steelblue', lty=2)
	  
x <- rnorm(1)
for (t in 2:1000) {
	x <- c(x, 0.09*x[t - 1] + rnorm(1))
}
plot(x, type='l', ylim=range(cumsum(x)))
abline(h = 0, col = 2)
lines(cumsum(x), col='orange3')

x <- matrix(0, ncol=100, nrow=10000)
for (t in 2:10000) {
	x[t, ] <- x[t - 1, ] + rnorm(100)*10^(-2)
}
plot(seq(0, 1, le=10000), x[, 1], ty='n', ylim=range(x))
polygon(c(1:10000, 10000:1) / 10000, c(apply(x, 1, max), rev(apply(x, 1, min))),
		col='gold')
polygon(c(1:10000, 10000:1) / 10000, c(apply(x, 1, quantile, 0.95), 
		rev(apply(x, 1, quantile, 0.05))), col='brown')


# 1.7 Writing New R Functions




save.image('~/Desktop/R/MonteCarlo/MC.RData')
