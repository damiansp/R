#===============================#
#                				#
# Ch 4 Basic Stochastic Models	#
#                				#
#===============================#

rm(list = ls())
load('~/Desktop/R/Time Series/TimeSeries.RData')
source('~/Desktop/SM/get.hist.quote2.R', chdir = TRUE)

sp = get.hist.quote2( '^gspc', #start = '2004-01-01', 
					  quote = 'AdjClose' )
sp = ts(as.ts(sp), start = 1991, frequency = 365)
sp2011 = get.hist.quote2( '^gspc', start = '2011-01-01', quote = 'AdjClose' )
sp2011 = ts(as.ts(sp2011), start = 2011, frequency = 365)
lode = get.hist.quote2('lode', start = '2011-01-01', quote = 'AdjClose')
lode = ts(as.ts(lode), start = 2011, frequency = 365)


# 4.1 Purpose



# 4.2 White Noise
	# 4.2.3 Simulation in R
	w = rnorm(100)
	plot(w, type = 'l')

	x = seq(-3, 3, length = 1000)
	hist(rnorm(1000), prob = T)
	lines(x, dnorm(x))

	# 4.2.4 Second-Order Properties and the Correlogram
	acf(rnorm(1000))



# 4.3 Random Walks
	# 4.3.7 Simulation
	x = w = rnorm(1000)
	for(t in 2:1000) { x[t] = x[t-1] + w[t] }
	par(mfrow = c(2, 1))
	plot(x, type = 'l')
	acf(x)



# 4.4 Fitted Models and Diagnostic Plots
	# 4.4.1 Siumlated random walk series
	acf(diff(x))

	# 4.4.2 Exchange rate series
	plot(Z.ts)
	plot(diff(Z.ts))
	acf(diff(Z.ts))

	Z.hw = HoltWinters(Z.ts, alpha = 1, gamma = 0)
	plot(Z.hw)
	acf(resid(Z.hw))

	Z.hw2 = HoltWinters(Z.ts)
	plot(Z.hw2)
	acf(resid(Z.hw2))

	# 4.4.3 Random walk with drift
	HP.dat = read.table(paste(web, 'HP.txt', sep = ''), header = T)
	attach(HP.dat)
	plot(as.ts(Price))
	DP = diff(Price)
	par(mfrow = c(2,1))
	plot(as.ts(DP)); abline(h = 0)
	mean(DP) + c(-2, 2)*sd(DP) / sqrt(length(DP))
	abline(h = mean(DP), col = 2)
	abline(h = mean(DP) + c(-2, 2)*sd(DP) / sqrt(length(DP)), col = 3, lty = 2)
	acf(DP)
	
	# Simulate data from model w HP parameters to see how well it applies
	par(mfrow = c(1, 1))
	n = dim(HP.dat)[1]
	plot(as.ts(Price), ylim = c(0, 80), xlim = c(0, n + 100))
	
	mu = mean(DP)
	sigma = sd(DP)
	
	x = numeric(n + 100)
	iters = 1000
	outM = matrix(0, (n + 100), iters)
	x[1] = HP.dat[1, ]
	
	for (i in 1:iters) {
		for (j in 2:(n + 100)) {
			x[j] = x[j - 1] + mu + rnorm(1, 0, sigma)
		}
		
		outM[, i] = x
		lines(x, col = rgb(0, 0.8, 1, alpha = 0.03))
		x[1] = HP.dat[1, ]
	}
	
	ci95 = apply(outM, 1, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
	lines(ci95[1,], lty = 1, col = rgb(0, 0.8, 1))
	lines(ci95[3,], lty = 1, col = rgb(0, 0.8, 1))
	lines(ci95[5,], lty = 1, col = rgb(0, 0.8, 1))
	lines(ci95[2,], lty = 2, col = rgb(0, 0.8, 1))
	lines(ci95[4,], lty = 2, col = rgb(0, 0.8, 1))
	
	# forecast
	x = numeric(100)
	outM = matrix(0, 100, iters)
	x[1] = HP.dat[n, ]

	for (i in 1:iters) {
		for (j in 2:100) {
			x[j] = x[j - 1] + mu + rnorm(1, 0, sigma)
		}
		
		outM[, i] = x
		lines(y = x, x = n:(n + 99), col = rgb(1, 0, 0.8, alpha = 0.03))
		x[1] = HP.dat[n, ]
	}
	
	ci95 = apply(outM, 1, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
	lines(x = n:(n + 99), y = ci95[1,], lty = 1, col = rgb(1, 0, 0.8))
	lines(x = n:(n + 99), y = ci95[3,], lty = 1, col = rgb(1, 0, 0.8))
	lines(x = n:(n + 99), y = ci95[5,], lty = 1, col = rgb(1, 0, 0.8))
	lines(x = n:(n + 99), y = ci95[2,], lty = 2, col = rgb(1, 0, 0.8))
	lines(x = n:(n + 99), y = ci95[4,], lty = 2, col = rgb(1, 0, 0.8))
	


# 4.5 Autoregressive Models
	# 4.5.1 Definition:
	# A series {x[t]} is autoregressive of order p: AR(p) ->
	# x[t] = alpha[1]x[t - 1] + alpha[2]x[t - 2] + ... + alpha[p]x[t - p] + w[t]
	# with {w[t]} as white noise, and alphas as parameters;
 	# equivlently:
 	# theta[p](B)x[t] = w[t] = 
 	#	(1 - alpha[1]B - alpha[2]B^2 - ... - alpha[p]B^p)x[t]
 	# NOTES: 
 	#	AR(1) is a special case = the random walk
 	#	Exponential smoothing is the special case where 
 	#	alpha[i] = alpha(1 - alpha)^i for i = 1, 2, ..., and p -> Inf
 	
 	
	# 4.5.3 Second-Order Properties of an AR(1) Model
	# AR(1): x[t] = alpha*x[t - 1] + w[t]; w[t] ~N(0, sigma^2);
	#	mean[x] = 0
	#	cov[k] = alpha^k * sigma^2 / (1 - alpha^2)
	
	# 4.5.5 Correlogram of an AR(1) process
	rho = function(k, alpha) alpha^k
	layout(1:2)
	plot( 0:10, rho(0:10, 0.7), type = 'l', xlab = 'k', 	
		  ylab = expression(rho[k]), main = expression(alpha == 0.7) )
	plot( 0:10, rho(0:10, -0.7), type = 'l', xlab = 'k', 
		  ylab = expression(rho[k]), main = expression(alpha == -0.7) )
	
	# 4.5.7 Simulation
	x = w = rnorm(1000)
	for(t in 2:1000) { x[t] = 0.7*x[t-1] + w[t] }
	plot(x, type = 'l')
	plot(x[1:100], type = 'l')
	acf(x)
	pacf(x)
	


# 4.6 Fitted Models
	# 4.6.1 Model fitted to simulated series
	plot(x, type = 'l')
	x.ar = ar(x, method = 'mle')
	x.ar$order	# 1
	x.ar$ar	# 0.68 (cf. with 0.7 in the specified model above)
	x.ar$ar + c(-2, 2)*sqrt(x.ar$asy.var)	# appx 95% CI [0.622, 0.716] 
											# (includes 0.7)

	# 4.6.2 Exchange rate series: fitted AR model
	Z.ar = ar(Z.ts)
	mean(Z.ts)	# 2.82
	Z.ar$order	# 1 (means Z.ar is AR(1))
	Z.ar$ar	# coefficient for AR term = 0.89
	Z.ar$ar + c(-2, 2) * sqrt(Z.ar$asy.var)	# appx 95%CI = [0.74, 1.04]
	acf(Z.ar$res[-1])
	#The model can now be reconstructed as:
	# z.hat[t] = mean + coef(z[t-1] - mean) or 
	# z.hat[t] = 2.82 + 0.89(z[t-1] - 2.8)
	Z.ar
	
	par(mfrow = c(3,1))
	plot(Z.ts)
	abline(h = mean(Z.ts), col = 'grey')		
	# begins at 2.92; length = 39
	t = numeric(39)
	t[1] = 2.92

	for(i in 2:39){
		t[i] = 2.82 + 0.89 * (t[i-1] - 2.8) + rnorm(1, 0, sd(Z.ts))
	}
	plot(t, type = 'l')
	abline(h = mean(t), col='grey')
	
	for(i in 2:39){
		t[i] = 2.82 + 0.89 * (t[i-1] - 2.8) + rnorm(1, 0, sd(Z.ts))
	}
	plot(t, type = 'l')
	abline(h = mean(t), col='grey')

	# 4.6.3 Global temperature series: fitted AR model
	# Global = scan("http://www.massey.ac.nz/~pscowper/ts/global.dat")
	Global.ts = ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
	Global.ar = ar(aggregate(Global.ts, FUN = mean), method = 'mle')
	mean(aggregate(Global.ts, FUN = mean))	# -0.138
	Global.ar$order	# 4 = AR(4); regressive over prev 4 time steps
	Global.ar$ar 	# coefs: 0.588, 0.126, 0.111, 0.268
	acf(Global.ar$res[-(1:Global.ar$order)], lag = 50)
	plot(Global.ts)
	
	plot(aggregate(Global.ts, FUN = mean), ylim = c(-1.5, 1))
	t = numeric(length(aggregate(Global.ts, FUN = mean)))
	t[1:4] = aggregate(Global.ts, FUN = mean)[1:4]
	mu = mean(aggregate(Global.ts, FUN = mean))
	stdev = sd(aggregate(Global.ts, FUN = mean))	

	iters = 1000

	for (iter in 1:iters) {
		for (i in 5:length(t)) {
			t[i] = mu + 0.588 * (t[i - 1] - mu) + 0.013 * (t[i - 2] - mu) +
						0.111 * (t[i - 3] - mu) + 0.268 * (t[i - 4] - mu) +
						rnorm(1, 0, stdev)
		}
	
		lines(seq(1856, 2005, length = length(t)), t, col = rgb(1, 0, 0, 0.1))
	}
	lines(aggregate(Global.ts, FUN = mean))	
	










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
plot(spNoNA, ylim = c(-1000, 3000))
	
t = numeric(length(spNoNA))
t[1] = spNoNA[1]
mu = mean(spNoNA)
stdev = sd(spNoNA)

iters = 1000

for (iter in 1:iters) {
	for (i in 2:length(t)) {
		t[i] = mu + 0.9998 * (t[i - 1] - mu) + rnorm(1, 0, stdev)
	}
	
	lines((1:6174), t, col = rgb(1, 0, 0, 0.05))
}
lines(spNoNA)	

for (i in 2:length(t)) {
	t[i] = mu + 0.9998 * (t[i - 1] - mu)
}
lines((1:6174), t, col = 4)





save.image(file = "~/Desktop/R/Time Series/TimeSeries.RData")
detach(HP.dat)
