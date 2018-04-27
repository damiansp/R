#===================#
#          			#
# Ch 5: Regression	#
#          			#
#===================#

rm(list = ls())
load('~/Desktop/R/Time Series/TimeSeries.RData')
source('~/Desktop/SM/get.hist.quote2.R', chdir = TRUE)

library(nlme)

sp = get.hist.quote2( '^gspc', #start = '2004-01-01', 
					 quote = 'AdjClose' )
sp = ts(as.ts(sp), start = 1991, frequency = 365)
sp2011 = get.hist.quote2( '^gspc', start = '2011-01-01', quote = 'AdjClose' )
sp2011 = ts(as.ts(sp2011), start = 2011, frequency = 365)
lode = get.hist.quote2('lode', start = '2011-01-01', quote = 'AdjClose')
lode = ts(as.ts(lode), start = 2011, frequency = 365)
web = 'http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/'

# 5.2 Linear Models
	# 5.2.3 Simualation
	z = w = rnorm(100, sd = 20)
	for(t in 2:100) { z[t] = 0.8 * z[t - 1] + w[t] }	
	Time = 1:100
	x = 50 + 3 * Time + z
	plot(x, xlab = 'time', type = 'l')
	# mod: x[t] = 50 + 3t + z[t] where {z[t]} is AR(1), 
	# 	z[t] = 0.8z[t - 1] + w[t], and w[t] is Gaussian white noise ~N(0, 20)
	


# 5.3 Fitted Models
	# 5.3.1 Model fitted to simulated data
	x.lm = lm(x ~ Time)
	coef(x.lm)	# 53.2, 2.99; cf 50 and 3 in specified model above
	sqrt(diag(vcov(x.lm)))	# 6.34, 0.109
							# = SE on coefs [intercept and time]), cf:
	summary(x.lm)	# note t-tests/significance not accurate here
	
	acf(resid(x.lm))
	pacf(resid(x.lm))	# lag 1 signif. indicates AR(1) in residuals

	#5.3.2 Model fitted to the temperature series (1970-2005)
	Global = scan(paste(web, 'global.dat', sep = ''))
	Global.ts = ts(Global, st = c(1856,1), end = c(2005,12), fr = 12)
	Global.annual = aggregate(Global.ts, FUN = mean)
	temp = window(Global.ts, start = 1970)
	temp.lm = lm(temp ~ time(temp))
	summary(temp.lm)	#again not that t-tests/signif not accurate here
	confint(temp.lm)	#too narrow bc of autocorrelation:
	acf(resid(temp.lm))



# 5.4 Generalized Least Squares
	# 5.4.1 GLS to simulated series
	x.gls = gls(x ~ Time, cor = corAR1(0.8))	# correlation specified in 
												# model n 5.2.3 above
	# NOTE: usually the AR(1) correlation will not be known. To estimate, 
	# plot as ordinary least squares (lm()), and read of AR(1) from 
	# acf(resid(lm.object))
	summary(x.gls)	# cf. coefs: intercept = 53.2, Time = 3.0
	sqrt(diag(vcov(x.gls)))	# same as SE in summary

	# 5.4.2 Confidence interval for the trend in the temperature series
	temp.gls = gls(temp ~ time(temp), cor = corAR1(0.7))	
	# cor from acf(resid(temp.lm))
	summary(temp.gls)
	confint(temp.gls)



# 5.5 Linear Models with Seasonal Variables
	# 5.5.3 Example: Seasonal Model for the Temperature Series
	Seas = cycle(temp)	# returns a vector of values 1:12 repeated for the 
						#length of the entire TS (1-12 representing the 
						# seasons/months)
	Time = time(temp)	# returns year data for each entry in TS
	temp.lm = lm(temp ~ 0 + Time + factor(Seas))
	# equivalent to: temp.lm = lm(temp ~ -1 + Time + factor(Seas))
	summary(temp.lm)

	# Predict 2 years ahead
	new.t = seq(2006, len = 2 * 12, by = 1 / 12)
	alpha = coef(temp.lm)[1]	# Time
	beta = rep(coef(temp.lm)[2:13], 2) # seasons
	(alpha * new.t + beta)[1:12]
	new.dat = data.frame(Time = new.t, Seas = rep(1:12, 2))
	(temp.pred = predict(temp.lm, new.dat))
	
	plot(temp, xlim = c(1970, 2008))
	lines(temp.pred ~ new.t, col = 2)


# 5.6 Harmonic Seasonal Models
TIME = seq(1, 12, len = 1000)
plot(TIME, sin(2 * pi * TIME / 12), type = 'l')
lines( TIME, sin(2 * pi * TIME / 12) + 0.2 * sin(2 * pi * 2 * TIME / 12) +
	  0.1 * cos(2 * pi * 4 * TIME / 12), 
	  col = 2)	# just an ex of type of seasonal variation that can be 
	  			# modeled
	# 5.6.1 Simulation
	# set.seed(1)
	TIME = 1:(10 * 12)	# ten periods of 12 seasons ea
	w = rnorm(10 * 12, sd = 0.5)

	Trend = 0.1 + 0.005 * TIME + 0.001 * TIME^2
	Seasonal = sin(2 * pi * TIME / 12) + 0.2 * sin(2 * pi * TIME / 12) + 
			   0.1 * sin(2 * pi * 4 * TIME / 12) + 
			   0.1 * cos(2 * pi * 4 * TIME / 12)
	x = Trend + Seasonal + w
	par(mfrow = c(3,1))
	plot(Trend ~ TIME, type = 'l')
	plot(Seasonal ~ TIME, type = 'l')
	plot((Trend + Seasonal) ~ TIME, type = 'l')
	lines(x, col = 2)
	
	#5.6.2 Fit to simulated series
	SIN = COS = matrix(nr = length(TIME), nc = 6)
	for(i in 1:6) {
		COS[, i] = cos(2 * pi * i * TIME / 12)
		SIN[, i] = sin(2 * pi * i * TIME / 12)
	}
	
	x.lm1 = lm( x ~ TIME + I(TIME^2) + COS[, 1] + SIN[, 1] + COS[, 2] + 
				SIN[, 2] + COS[, 3] + SIN[, 3] + COS[, 4] + SIN[, 4] + 
				COS[, 5] + SIN[, 5] + COS[, 6] + SIN[, 6])
	summary(x.lm1)
	
	coef(x.lm1) / sqrt(diag(vcov(x.lm1)))	# any w/ value ≥2 acts a proxy 
											# for signif at 0.05
	x.lm2 = lm(x ~ I(TIME^2) + SIN[, 1] + SIN[, 3] + COS[, 4])
	coef(x.lm2) / sqrt(diag(vcov(x.lm2)))
	summary(x.lm2)

	xv = 0:125
	yv = 0.31 + 0.001*xv^2 + 1.1*sin(2*pi*xv/12) + 1.5*sin(6*pi*xv/12) +
		 2.3*cos(8*pi*xv/12)
	lines(xv, yv, col = 4)
	AIC(x.lm1)	# 191.85
	AIC(x.lm2)	# 173.83
	
	x.lm3 = step(x.lm1, direction = 'both')
	summary(x.lm3)
	yv = 0.30 + 0.001*xv^2 + 1.1*sin(2*pi*xv/12) + 1.5*sin(6*pi*xv/12) +
		 2.3*cos(8*pi*xv/12) + 1.1*sin(8*pi*xv/12)
	lines(yv, col = 5)

	# 5.6.3 Harmonic model fitted to temperature series (1970-2005)
	SIN = COS = matrix(nr = length(temp), nc = 6)
	for(i in 1:6) {
		COS[,i] = cos(2*pi*i*temp/12)
		SIN[,i] = sin(2*pi*i*temp/12)
	}
	TIME = (time(temp) - mean(time(temp))) / sd(time(temp))
	mean(time(temp))
	sd(time(temp))
	temp.lm1 = lm( temp ~ TIME + I(TIME^2) + COS[,1] + SIN[,1] + COS[,2] + 
				   SIN[,2] + COS[,3] + SIN[,3] + COS[,4] + SIN[,4] + 
				   COS[,5] + SIN[,5] + COS[,6] + SIN[,6] )
	summary(temp.lm1)
	coef(temp.lm1) / sqrt(diag(vcov(temp.lm1)))
	temp.lm2 = step(temp.lm1, direction = 'both')
	summary(temp.lm2)


	plot(temp)
	par(mfrow = c(3,1))
	plot(time(temp), resid(temp.lm2), type = 'l')
	abline(h = 0, col = 2)
	acf(resid(temp.lm2))
	pacf(resid(temp.lm2))



#5.7 Logarithmic Transformations
	#5.7.2 Example using the air passenger series
	data(AirPassengers)
	AP = AirPassengers
	plot(AP)
	plot(log(AP))

	SIN = COS = matrix(nr = length(AP), nc = 6)
	for(i in 1:6) {
		SIN[,i] = sin(2*pi*i*time(AP))
		COS[,i] = cos(2*pi*i*time(AP))
	}
	TIME = (time(AP) - mean(time(AP))) / sd(time(AP))
	mean(time(AP))	# 1955

	sd(time(AP))	# 3.476
	AP.lm1 = lm( log(AP) ~ TIME + I(TIME^2) + I(TIME^3) + I(TIME^4) + 
				 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] + COS[,3] + SIN[,3] + 
				 COS[,4] + SIN[,4] + COS[,5] + SIN[,5] + COS[,6] + SIN[,6] )
	AP.lm2 = step(AP.lm1, direction = 'both')
	summary(AP.lm2)
	AIC(AP.lm1)	# -447.95
	AIC(AP.lm2)	# -453.46
	acf(resid(AP.lm2))
	pacf(resid(AP.lm2))
	pacf(resid(AP.lm2))$acf	# 0.644

	AP.gls = gls( log(AP) ~ TIME + I(TIME^2) + I(TIME^4) + COS[,1] + 		
				  SIN[,1] + COS[,2] + SIN[,2] + COS[,3] + SIN[,3] + COS[,4] + 
				  SIN[,4] + SIN[,5], cor = corAR1(0.644) )
	summary(AP.gls)
	AP.ar = ar(resid(AP.lm2), order = 1, method = 'mle')
	AP.ar$ar	# 0.646
	acf(AP.ar$res[-1])



#5.8 Non-Linear Models
	#5.8.2 Example of a simulated and fitted non-linear series
	#set.seed(1)
	w = rnorm(100, sd = 10)
	z = numeric(100)
	for(t in 2:100) { z[t] = 0.7 * z[t-1] + w[t] }	# AR(1) error terms
	Time = 1:100
	f = function(x) { exp(1 + 0.05 * x) }
	x = f(Time) + z
	plot(x, type = 'l')
	abline(h = 0)

	x.nls = nls( x ~ exp(alp0 + alp1 * Time), 
				 start = list(alp0 = 0.1, alp1 = 0.5) )
	summary(x.nls)



# 5.9 Forecasting from Regression
	# 5.9.2 Prediction in R
	new.t = time(ts(start = 1961, end = c(1970,12), fr = 12))
	TIME = (new.t - mean(time(AP))) / sd(time(AP))
	SIN = COS = matrix(nr = length(new.t), nc = 6)
	for(i in 1:6) {
		SIN[,i] = sin(2*pi*i*new.t)
		COS[,i] = cos(2*pi*i*new.t)
	}
	SIN = SIN[,-6]
	new.dat = data.frame(TIME = as.vector(TIME), SIN = SIN, COS = COS)
	AP.pred.ts = exp(ts(predict(AP.lm2, new.dat), st = 1961, fr = 12))
	par(mfrow = c(2,1))
	ts.plot(log(AP), log(AP.pred.ts), col = 1:2)
	ts.plot(AP, AP.pred.ts, col = 1:2)



 # 5.10 Inverse Transform and Bias Correction
	# 5.10.3 Example useing the air passenger data
	summary(AP.lm2)	# r.sq = 0.989
	sigma = summary(AP.lm2)$sigma	# resid SE
	(lognorm.correction.factor = exp((1/2)*sigma^2)) #1.0011
	(empirical.correction.factor = mean(exp(resid(AP.lm2)))) #1.0010
	AP.pred.ts = AP.pred.ts*empirical.correction.factor
	par(mfrow = c(1,1))
	ts.plot(log(AP), log(AP.pred.ts), col = 1:2)









# Playing with S&P data
plot(sp)
sp.daily.vals = (as.vector(sp[!is.na(sp)]))
n = length(sp.daily.vals)
sp.daily.change = sp.daily.vals[2:n] / sp.daily.vals[1:(n - 1)]
plot(sp, xlim = c(1991, 2019), ylim = c(0, 5000))
iters = 1000
n.forecast = 1000
lastDate = attr(sp, 'tsp')[2]
dates = seq(lastDate, lastDate + 4, length = n.forecast)

fillNA = function(x) {
	nas = which(is.na(x))
	while(length(nas) > 0) {
		x[nas] = x[nas - 1]
		nas = which(is.na(x))
	}
	
	return(x)
}

spFill = fillNA(sp)

M = matrix(NA, nrow = n.forecast, ncol = iters)
for (i in 1:iters) {
	sp.forecast = numeric(n.forecast)
	end = length(sp.daily.change)
	rand.start = sample(1:(end - 365), 1)
	samp.period = sp.daily.change[rand.start:(rand.start + 365)]
	daily.changes = sample(samp.period, n.forecast, T)
	sp.forecast[1] = sp.daily.vals[n] * daily.changes[1]
	for (p in 2:n.forecast) {
		sp.forecast[p] = sp.forecast[p - 1] * daily.changes[p]
	}
	lines(sp.forecast ~ dates, col = rgb(0,0,1,0.02))
	M[, i] = sp.forecast
}
qs = apply(M, 1, quantile, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
lines(qs[1,] ~ dates, col = 1, lty = 1)
lines(qs[4,] ~ dates, col = 1, lty = 1)
lines(qs[7,] ~ dates, col = 1, lty = 1)

lines(qs[2,] ~ dates, col = 2, lty = 3)
lines(qs[6,] ~ dates, col = 2, lty = 3)

lines(qs[3,] ~ dates, col = 2, lty = 2)
lines(qs[5,] ~ dates, col = 2, lty = 2)
abline(h = 0, col = 'grey')

par(mfrow = c(1,1))

decAdd = decompose(spFill)
trendAdd = decAdd$trend
seasAdd = decAdd$seasonal
eAdd = decAdd$random

decMult = decompose(spFill, type = 'mult')
trendMult = decMult$trend
seasMult = decMult$seasonal
eMult = decMult$random

sd(eAdd, na.rm = T)	# 45.20
sd(eMult, na.rm = T)	# 0.041

plot(decompose(spFill))
plot(decompose(spFill, type = 'multiplicative'))

par(mfrow = c(2, 1))
plot(sp)
lines(trendAdd + seasAdd, col = 2)
plot(sp)
lines(trendMult + seasMult, col = 2)

plot(sp, xlim = c(2010, 2016))
lines(trendAdd + seasAdd, col = 2)
plot(sp, xlim = c(2010, 2016))
lines(trendMult + seasMult, col = 2)

sp.hw = HoltWinters(sp.daily.vals, beta = F, gamma = F) 
plot(sp.hw)
plot(sp.hw, xlim = c(6100, 6160), ylim = c(2050, 2150))
sp.hw	# smoothing param = 0.93 or almost 1, meaning it basically just 	
		# predicts that tomorrows val = today's value... of no use
		
SP.hw = HoltWinters(ts(sp.daily.vals, frequency = 250), seasonal = "mult")
plot(SP.hw)
plot(SP.hw, xlim = c(25, 27))
SP.predict = predict(SP.hw, n.ahead = 1*250)
ts.plot(ts(sp.daily.vals, frequency = 250), SP.predict, col = 1:2)
ts.plot( ts(sp.daily.vals, frequency = 250), SP.predict, col = 1:2, 
		 xlim = c(25.5, 26.75), ylim = c(500, 2500) )




spNoNA = ts(sp[!is.na(sp)])
sp2011NoNA = ts(sp2011[!is.na(sp2011)])
lodeNoNA = ts(lode[!is.na(lode)])
acf(spNoNA)
acf(diff(spNoNA))
acf(decompose(spFill)$random[183:8731])
acf(diff(decompose(spFill)$random[183:8731]))

ts.plot(sp2011, lode*350, col = 1:2)

acf(cbind(diff(sp2011NoNA), diff(lodeNoNA)))



sp.ar = ar(spNoNA, method = 'mle')
sp.ar$order	# 12
cbind( sp.ar$ar + c(-2)*sqrt(x.ar$asy.var), 
	   sp.ar$ar, 
	   sp.ar$ar + c(2)*sqrt(x.ar$asy.var) )
sp.ar = ar(spNoNA, method = 'mle', order.max = 1)
sp.ar


par(mfrow = c(1, 1))
plot(spNoNA, ylim = c(-5000, 7000))
	
t = numeric(length(spNoNA))
t[1] = spNoNA[1]
mu = mean(spNoNA)
stdev = sd(spNoNA)

iters = 1000

for (iter in 1:iters) {
	for (i in 2:length(t)) {
		t[i] = mu + 0.9998 * (t[i - 1] - mu) + rnorm(1, 0, stdev)
	}
	
	lines((1:length(t)), t, col = rgb(1, 0, 0, 0.01))
}
lines(spNoNA)	

for (i in 2:length(t)) {
	t[i] = mu + 0.9998 * (t[i - 1] - mu)
}
lines((1:length(t)), t, col = 4)





save.image(file = "~/Desktop/R/Time Series/TimeSeries.RData")
