########################################
#                                      #
#  1 Financial Markets, Prices & Risk  #
#                                      #
########################################

load('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
library(tseries)
library(moments)
library(MASS)
library(stats)
library(car)
options(digits=4)

#1.2 S&P 500 Returns
	#1.2.2 S&P 500 statistics in R and Matlab
price <- get.hist.quote("^GSPC", start="2000-01-01", quote="AdjClose")
y <- diff(log(price))	#converts prices to returns

par(mfrow=c(2,1))
plot(price)
plot(y, type='l')
y <- coredata(y)	#strips date info from returns

sd(y)	#0.0137
min(y)	#-0.0947
max(y)	#0.1096
skewness(y)	#-0.1595
kurtosis(y)	#10.17
truehist(y)
x <- seq(-0.1,0.1,0.001)
lines(x, dnorm(x, mean(y), sd(y)))


par(mfrow=c(1,1))
acf(y)
acf(y^2)	#proxy for volatility
jarque.bera.test(y)	#a normality test using skewness and kurtosis
Box.test(y, lag=20, type=c("Ljung-Box"))	#tests for independence in a TS
Box.test(y^2, lag=20, type=c('Ljung-Box'))



#1.4 Volatility
	#1.4.2 Volatility clusters and the ACF
q <- acf(y)
q <- acf(y^2)
b <- Box.test(y, lag=21, type='Ljung-Box')



#1.6 Identification of Fat Tails
	#1.6.2 Graphical methods for fat-tail analysis
qqnorm(y); qqline(y)
qq.plot(y, envelope=F)
qq.plot(y, distribution="t", df=5)



#1.7 Nonlinear Dependence
	#1.7.1 Sample evidence of nonlinear dependence
MSFT.price <- get.hist.quote(instrument="MSFT", start='2007-06-01', end='2009-12-31', quote='AdjClose')	#Microsoft
MS.price <- get.hist.quote(instrument="MS", start='2007-06-01', end='2009-12-31', quote='AdjClose')	#Morgan Stanley
GS.price <- get.hist.quote(instrument="GS", start='2007-06-01', end='2009-12-31', quote='AdjClose')	#Goldman-Sachs
p <- cbind(MSFT.price, MS.price, GS.price)
par(mfrow=c(2,1))
matplot(log(p), type='l', lty=1)
matplot(diff(log(p)), type='l', lty=1)
y <- diff(log(p))
cor(y)





save.image('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
