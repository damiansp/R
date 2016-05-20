########################################
#                                      #
#  3 Multivariate Volatility Modeling  #
#                                      #
########################################

load('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
library(tseries)
library(moments)
library(MASS)
library(stats)
library(car)
library(fGarch)
library(gogarch)
library(ccgarch)
options(digits=4)

#3.1 Multivariate Volatility Forecasting
	#3.1.1 Application
msft <- get.hist.quote('msft', start='2000-01-01', quote='AdjClose')
ibm <- get.hist.quote('ibm', start='2000-01-01', quote='AdjClose')
plot(ibm, type='l', ylim=c(0,220))
lines(msft, col=2)
p <- cbind(msft, ibm)
y <- diff(log(p)) * 100
plot(y)
y[,1] <- y[,1] - mean(y[,1])
y[,2] <- y[,2] - mean(y[,2])
T <- length(y[,1])



#3.2 EWMA (Exponentially Weighted Moving Average)
EWMA <- matrix(nrow=T, ncol=3)
lambda <- 0.94
S <- cov(y)
EWMA[1,] <- c(S[1,1], S[2,2], S[1,2])	#var(msft), var(ibm), cov(msft, ibm)
for(i in 2:T) {
	S <- lambda * S + (1 - lambda) * t(y[i]) %*% y[i]
	EWMA[i,] <- c(S[1,1], S[2,2], S[1,2])
	}
EWMArho <- EWMA[,3] / sqrt(EWMA[,1] * EWMA[,2])	#correlations



#3.3 Orthogonal GARCH
res <- gogarch(y, formula = ~ garch(1,1), garchlist=c(include.mean=F))
OOrho <- ccor(res)



#3.4 CCC and DCC Models
	#3.4.3 Implementation
f1 <- garchFit(~garch(1,1), data=y[,1], include.mean=F)
f1 <- f1@fit$coef
f2 <- garchFit(~garch(1,1), data=y[,2], include.mean=F)
f2 <- f2@fit$coef

a <- c(f1[1], f2[1])
A <- diag(c(f1[2], f2[2]))
B <- diag(c(f1[3], f2[3]))
dccpara <- c(0.2, 0.6)

dccresults <- dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara, dvar=y, model='diagonal')
dccresults$out
DCCrho <- dccresults$DCC[,2]

matplot(cbind(EWMArho, DCCrho, OOrho), type='l', lty=1)
abline(h=0.49)
legend('bottomleft', c('EWMA', 'DCC', 'OOrho'), lty=c(1,1,1), col=1:3)


save.image('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')

