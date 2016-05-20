######################
#                    #
# Formulas from text #
#                    #
######################

load('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
library(tseries)
library(moments)
library(MASS)
library(stats)
library(car)
options(digits=4)

#Simple Moving Average Volatility
movingAvgVolatility <- function(returns, window) {
	vol <- (1 / window) * sum(returns[(length(returns) - window + 1):length(returns)]^2)
	vol
	}

#Exponentially Weighted Moving Average Volatility
	#lambda < 1 are weights
ewMovAvgVolatility <- function(returns, window, lambda) {
	coef <- ((1 - lambda) / (lambda*(1 - lambda^window)))
	lambda.v <- lambda^(1:window)
	sum.v <- lambda.v * returns[length(returns):(length(returns) - window + 1)]^2
	vol <- coef * sum(sum.v) 
	vol
	}







save.image('/Users/damiansp/Desktop/Hackery/R Files/Financial Risk Forecasting/FFR.RData')
