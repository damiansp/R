#=======================================#
#										#
#  Regression							#
# 	Coursera course via Johns Hopkins	#
#										#
#=======================================#

rm(list=ls())
library(UsingR)		# galton dataset
library(datasets)
# library(manipulate)	# R studio only
data(diamond)
data(galton)
data(swiss)

require(graphics)

load('~/Desktop/R/RegressionClass/regressionClass.RData')


# Basic Least Squares I
par(mfrow=c(1, 2))

truehist(galton$child)
truehist(galton$parent)

plot(galton$child ~ galton$parent)


# Basic Least Squares II
myHist <- function(mu) {
	hist(galton$child, col='blue', breaks=100)
	lines(c(mu, mu), c(0, 150), col=2, lwd=5)
	mse <- mean((galton$child - mu)^2)
	text(63, 150, paste('mu: ', mu))
	text(63, 140, paste('mse: ', round(mse, 2)))
}

slider <- seq(62, 74, 0.5)
length(slider)
par(mfrow=c(5,5))

for (s in slider) {
	myHist(s)
}

# Regression Through the Origin
plot(jitter(galton$parent), jitter(galton$child))
fit <- lm(galton$child ~ galton$parent)
abline(fit)
fit0 <- lm(galton$child ~ galton$parent - 1)
abline(fit0, col=2)
summary(fit)

plot(diamond$carat, diamond$price, bg='lightblue', pch=21, frame=F)
fit <- lm(price ~ carat, data=diamond)
abline(fit)

newx <- c(0.16, 0.27, 0.34)
predict(fit, newdata=data.frame(carat=newx))

# Residuals
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y - yhat)))
plot(fit)

# Residual Variation
summary(fit)
summary(fit)$sigma	# residual sd ('Residual standard error') =
sqrt(sum(resid(fit)^2) / (n - 2))

# Tests for Regression Coefficients
beta1 <- cor(x, y) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n - 2))
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x)^2 / ssx)^ 0.5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df=n - 2, lower.tail=F)
pBeta1 <- 2 * pt(abs(tBeta1), df=n - 2, lower.tail=F)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c('Est', 'SE', 't', 'P(>|t|)')
rownames(coefTable) <- c('Intercept', 'x')
coefTable
summary(fit)
# get CIs
sumCoef <- summary(fit)$coefficients
sumCoef[1, 1] + c(-1, 1) * qt(0.975, df=fit$df) * sumCoef[1, 2]	# intercept
sumCoef[2, 1] + c(-1, 1) * qt(0.975, df=fit$df) * sumCoef[2, 2]	# x

# Prediction Intervals
plot(x, y, xlab='Carat', ylab='Singapore $', xlim=c(0, 0.5), ylim=c(0, 2000))
abline(fit)
xVals <- seq(0, 0.5, by=0.01)
yVals <- beta0 + beta1 * xVals
se1 <- sigma * sqrt(1 / n + (xVals - mean(x))^2 / ssx)
se2 <- sigma * sqrt(1 + 1 / n + (xVals - mean(x))^2 / ssx)	# Prediction SE
lines(xVals, yVals + 2*se1, col=4)
lines(xVals, yVals - 2*se1, col=4)
lines(xVals, yVals + 2*se2, col=2)
lines(xVals, yVals - 2*se2, col=2)
# same as:
newdata <- data.frame(x=xVals)
p1 <- predict(fit, newdata, interval='confidence')
p2 <- predict(fit, newdata, interval='prediction')
plot(x, y, xlab='Carat', ylab='Singapore $', xlim=c(0, 0.5), ylim=c(0, 2000))
abline(fit)
lines(xVals, p1[, 2], col=4); lines(xVals, p1[, 3], col=4)
lines(xVals, p2[, 2], col=2); lines(xVals, p2[, 3], col=2)

# Multivariable Linear Models Interpretation
n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- x + x2 + x3 + rnorm(n, sd = 0.1)
e <- function (a, b) { a - sum(a * b) / sum(b^2) * b }
ey <- e(e(y, x2), e(x3, x2))
ex <- e(e(x, x2), e(x3, x2))
sum(ey * ex) / sum(ex^2)

summary(lm(y ~ x + x2 + x3 - 1))

# Multivariable Regression Examples
pairs(swiss, panel=panel.smooth, col=3 + (swiss$Catholic > 50))
swiss.m <- lm(Fertility ~ ., data=swiss)
swiss.m <- step(swiss.m, direction='both')
summary(swiss.m)


save.image('~/Desktop/R/RegressionClass/regressionClass.RData')