#===============================#
#                               #
#  Ch 7: Non-Stationary Models  #
#                               #
#===============================#

rm(list = ls())
load('~/Desktop/R/Time Series/TimeSeries.RData')
source('~/Desktop/SM/get.hist.quote2.R')

#options(digits = 5)
library(nlme)
library(MASS)
library(tseries)

# 7.2 Non-Seasonal ARIMA Models
	# 7.2.1 Differencing and the electricity series
	layout(c(1,1,2,3))
	plot(Elec.ts)
	plot(diff(Elec.ts))
	plot(diff(log(Elec.ts)))

	# 7.2.4 Simulation and Fitting
	#set.seed(1)
	x = w = rnorm(1000)
	for(i in 3:1000) {
		x[i] = 0.5*x[i - 1] + x[i - 1] - 0.5*x[i - 2] + w[i] + 0.3*w[i - 1]
	}
	arima(x, order = c(1, 1, 1))	# ar1: 0.42 ± 0.04; ma1: 0.33 ± 0.05
	plot(x, type = 'l')
	
	# Simulation can be explicit as above, or with arima.sim()
	x = arima.sim(model = list(order = c(1, 1, 1), ar = 0.5, ma = 0.3), 
				  n = 1000)
	plot(x, type = 'l', col = rgb(0, 0, 0, 0.1), ylim = c(-200, 200))
	
	for (i in 1:10000) {
		x = arima.sim(model = list(order = c(1, 1, 1), ar = 0.5, ma = 0.3), 
					  n = 1000)
		lines(x, col = rgb(0, 0, 0, 0.01))
	}

	# 7.2.5 IMA(1,1) model fitted to the beer production series
	Beer.ts = ts(CBE[,2], start = 1958, freq = 12)
	plot(Beer.ts)
	plot(diff(Beer.ts))
	Beer.ima = arima(Beer.ts, order = c(0, 1, 1))
	Beer.ima
	# x[t] = x[t - 1] + w[t] - 0.333*w[t - 1]
	acf(resid(Beer.ima))

	Beer.1991 = predict(Beer.ima, n.ahead = 24)
	plot(Beer.ts, xlim = c(1958, 1993))
	lines(seq(1991, 1992 + (11 / 12), length.out = 24), Beer.1991$pred,
		  col = 2)



	# 7.3 Seasonal ARIMA Models
		# 7.3.2 Fitting procedure
		AIC(arima(log(Elec.ts), order = c(1, 1, 0), 
				  seas = list(order = c(1, 0, 0), 12)))
		AIC(arima(log(Elec.ts), order = c(0, 1, 1), 
				  seas = list(order = c(0, 0 , 1), 12)))

		get.best.arima = function(x.ts, maxord=c(1, 1, 1, 1, 1, 1)) {
			best.aic = 1e8
			n = length(x.ts)
			for(p in 0:maxord[1]) {
				for(d in 0:maxord[2]) {
					for(q in 0:maxord[3]) {
						for(P in 0:maxord[4]) {
							for(D in 0:maxord[5]) {
								for(Q in 0:maxord[6]) {
									fit = arima(
										x.ts, 
										order = c(p, d, q), 
										seas = list(order = c(P, D, Q), 
													frequency(x.ts)),
										method='CSS'
									)

									fit.aic = -2 * fit$loglik + 
										(log(n) + 1) * length(fit$coef)

									if(fit.aic < best.aic) {
										best.aic = fit.aic
										best.fit = fit
										best.model = c(p,d,q, P,D,Q)
									}
								}
							}
						}
					}
				}
			}
			
			list(best.aic=best.aic, best.fit=best.fit, best.model=best.model)
		}

		best.arima.elec = get.best.arima(log(Elec.ts), 
										 maxord = c(2, 2, 2, 2, 2, 2))
		best.arima.elec
		best.fit.elec = best.arima.elec[[2]]
		acf(resid(best.arima.elec$best.fit))
		
		ts.plot(cbind(window(Elec.ts, start = 1981), 
					  exp(predict(best.fit.elec, 12)$pred)), col = 1:2)



# 7.4 ARCH Models
	# 7.4.1 S&P500 series
	data(SP500)
	plot(SP500, type = 'l')
	acf(SP500)
	abline(h = mean(SP500), col = 'red')
	acf((SP500 - mean(SP500))^2) # correlations in variance (volatility)

	# 7.4.4 Simulation and fitted GARCH model
	# Simulate:
	alpha0 = 0.1
	alpha1 = 0.4
	beta1 = 0.2	# note: alpha1 + beta1 must be < 1 to ensure stability
	w = rnorm(10000)
	a = h = numeric(10000)

	for(i in 2:10000) {
		h[i] = alpha0 + alpha1*(a[i - 1]^2) + beta1*h[i - 1]
		a[i] = w[i]*sqrt(h[i])
	}

	plot(a, type = 'l')
	acf(a)
	acf(a^2)

	# Estimate model parameters from simulated data
	a.garch = garch(a, grad = 'numerical', trace = F)
	a.garch2.2 = garch(a, grad = 'numerical', order = c(2, 2), trace = F)
	a.garch1.2 = garch(a, grad = 'numerical', order = c(1, 2), trace = F)
	a.garch2.1 = garch(a, grad = 'numerical', order = c(2, 1), trace = F)
	summary(a.garch)	#AIC: 13261
	summary(a.garch2.2)	#AIC: 13265
	summary(a.garch1.2)	#AIC: 13263
	summary(a.garch2.1)	#AIC: 13263
	confint(a.garch)

	# 7.4.5 Fit to S&P500 series
	sp.garch = garch(SP500, trace = F)
	sp.res = sp.garch$res[-1]
	acf(sp.res)
	acf(sp.res^2)

	# 7.4.6 Volatility in climate series
	stemp = scan(paste(web, 'stemp.dat', sep = ''))
	stemp.ts = ts(stemp, start = 1850, freq = 12)
	plot(stemp.ts)

	stemp.best = get.best.arima(stemp.ts, maxord=rep(2, 6))
	stemp.best
	stemp.arima = arima(stemp.ts, order = c(1, 1, 2), 
						seas = list(order = c(2, 0, 1), 12))
	t(confint(stemp.arima))
	
	stemp.res = resid(stemp.arima)
	layout(1:2)
	acf(stemp.res)
	acf(stemp.res^2)
	
	stemp.garch = garch(stemp.res, trace = F)
	t(confint(stemp.garch))
	
	stemp.garch.res = resid(stemp.garch)[-1]
	acf(stemp.garch.res)
	acf(stemp.garch.res^2)
	
	
	
	
		
	save.image('~/Desktop/R/Time Series/TimeSeries.RData')