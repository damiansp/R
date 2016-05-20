######################################
#                                    #
#  2 Univariate Volatility Modeling  #
#                                    #
######################################

load('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
library(tseries)
library(moments)
library(MASS)
library(stats)
library(car)
library(fGarch)
options(digits=4)

p <- get.hist.quote("^GSPC", start="2005-01-01", quote="AdjClose", quiet=T)
par(mfrow=c(2,1))
plot(p)
y <- diff(log(p)) * 100
y <- y - mean(y)
plot(y)
g.1.0 <- garchFit(~garch(1,0), data=y, include.mean=F)
summary(g.1.0)	#AIC: 3.442
g.4.0 <- garchFit(~garch(4,0), data=y, include.mean=F)
summary(g.4.0)	#AIC: 3.044
g.4.1 <- garchFit(~garch(4,1), data=y, include.mean=F)
summary(g.4.1)	#AIC: 2.926
g.1.1 <- garchFit(~garch(1,1), data=y, include.mean=F)
summary(g.1.1)	#AIC: 2.940
g.1.1.std <- garchFit(~garch(1,1), data=y, include.mean=F, cond.dist="std", trace=F)
summary(g.1.1.std)	#AIC: 2.903
res <- garchFit(~garch(1,1), data=y, include.mean=F, cond.dist='sstd', trace=F)
summary(res)	#AIC: 2.890
plot(res)

aparch.1.1 <- garchFit(~ aparch(1,1), data=y, include.mean=F, trace=F)
summary(aparch.1.1)	#AIC: 2.896
aparch.1.1.d2 <- garchFit(~ aparch(1,1), data=y, include.mean=F, trace=F, include.delta=F, delta=2)
summary(aparch.1.1.d2)	#AIC: 2.895
aparch.1.1.std <- garchFit(~ aparch(1,1), data=y, include.mean=F, trace=F, cond.dist="std")
summary(aparch.1.1.std)	#AIC: 2.864
aparch.2.2 <- garchFit(~ aparch(2,2), data=y, include.mean=F, trace=F)
summary(aparch.2.2)	#AIC: 2.890




save.image('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
