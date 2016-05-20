#===============================#
#                               #
#  Ch 3 Forecasting Strategies  #
#                               #
#===============================#
rm(list=ls())
load('~/Desktop/R/Time Series/TimeSeries.RData')
source('~/Desktop/SM/get.hist.quote2.R', chdir = TRUE)

sp <- get.hist.quote2( '^gspc', #start='2004-01-01', 
					   quote='AdjClose' )
sp <- ts(as.ts(sp), start=1991, frequency=365)
sp2011 <- get.hist.quote2( '^gspc', start='2011-01-01', quote='AdjClose' )
sp2011 <- ts(as.ts(sp2011), start=2011, frequency=365)
lode <- get.hist.quote2('lode', start='2011-01-01', quote='AdjClose')
lode <- ts(as.ts(lode), start=2011, frequency=365)

web <- 'http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/'


#3.2 Leading Variables and Associated Variables
	#3.2.2 Building approvals publication
Build.dat <- read.table(paste(web, 'ApprovActiv.dat', sep=''), header=T)
attach(Build.dat)
App.ts <- ts(Approvals, start=c(1996,1), freq=4)
Act.ts <- ts(Activity, start=c(1996,1), freq=4)
ts.plot(App.ts, Act.ts, col=1:2)

acf(ts.union(App.ts, Act.ts))

app.ran <- decompose(App.ts)$random
app.ran.ts <- window(app.ran, start=c(1996,3))
act.ran <- decompose(Act.ts)$random
act.ran.ts <- window(act.ran, start=c(1996,3))
dim(ts.union(app.ran.ts, act.ran.ts))
acf(ts.union(app.ran.ts, act.ran.ts)[-(40:41), ])
ccf(app.ran.ts[-(40:41)], act.ran.ts[-(40:41)])
print(acf(ts.union(app.ran.ts, act.ran.ts)[-(40:41), ]))



#3.3 Bass Model
	# 3.3.2 Model Definition
	# @param n0	number sold at time = 0
	# @param m	total number ultimately sold
	# @param p	coef. of innovation (how novel the item is)
	# @param q	coef. of imitation (how likely/quickly consumers are to mimic)
	# @param periods no. of time steps to plot/calculate
	bass <- function(n0, m, p, q, periods) {
		n <- numeric(periods)
		n[1] <- n0
		
		for (t in 2:periods) {
			n[t] <- n[t - 1] + p*(m - n[t - 1]) + 
					q*n[t - 1]*(m - n[t - 1]) / m
		}
		
		return (n)
	}
	
	n0 <- 10 	# no. users at time 0
	m <- 1000	# total n who will ultimately buy
	p <- 0.9		# coef. of innovation
	q <- 0.5 	# coef. of imitiation
	plot(bass(n0, m, p, q, 30), type='l')
	p <- 0.2
	lines(bass(n0, m, p, q, 30), type='l', col=2)
	q <- 0.2
	lines(bass(n0, m, p, q, 30), type='l', col=3)
	p <- 0.001; q <- 0.7
	lines(bass(n0, m, p, q, 30), type='l', col=4)
	p <- 0.7; q <- 0.001
	lines(bass(n0, m, p, q, 30), type='l', col=5)
	p <- 0.01; q <- 0.01
	lines(bass(n0, m, p, q, 30), type='l', col=6)
	
	p <- 0; q <- 0
	plot(bass(n0, m, p, q, 20), type='l', ylim=c(0, 1000))
	p <- 1	
	lines(bass(n0, m, p, q, 20), type='l', col=2)
	q <- 1
	lines(bass(n0, m, p, q, 20), type='l', col=3)
	p <- 0
	lines(bass(n0, m, p, q, 20), type='l', col=4)
	
	#3.3.4 Example	
	T79 <- 1:10
	Tdelt <- (1:100) / 10
	Sales <- c(840, 1470, 2110, 4000, 7590, 10950, 10530, 9470, 7790, 5890)
	Cusales <- cumsum(Sales)
	Bass.nls <- nls( Sales ~ M * (((P + Q)^2 / P) * exp(-(P + Q) * T79)) / 
							 (1 + (Q / P) * exp(-(P + Q) * T79))^2, 
					 start=list(M=60630, P=0.03, Q=0.38) )
	summary(Bass.nls)

	Bcoef <- coef(Bass.nls)
	m <- Bcoef[1]
	p <- Bcoef[2]
	q <- Bcoef[3]
	ngete <- exp(-(p + q) * Tdelt)
	Bpdf <- m * (( p + q)^2 / p) * ngete / (1 + (q / p) * ngete)^2
	plot( Tdelt, Bpdf, xlab="Years (since 1979)", ylab='Sales per year', 
		  type='l' )
	points(T79, Sales)
	Bcdf <- m* (1 - ngete) / (1 + (q / p)*ngete)
	plot(Tdelt, Bcdf, col=2, type='l')
	lines(Tdelt, Bpdf, xlab="Years (since 1979)", ylab='Cumulative sales')
	points(T79, Sales)
	points(T79, Cusales, col=2)



#3.4 Exponential Smoothing & the Holt-Winters Method
	#3.4.1 Exponential smoothing
	Motor.dat <- read.table(paste(web, 'motororg.dat', sep=''), header=T)
	attach(Motor.dat)
	mean(complaints) #19.4
	Comp.ts <- ts(complaints, start=c(1996, 1), freq=12)
	plot(Comp.ts, xlab='Time (months)', ylab='Complaints')

	Comp.hw1 <- HoltWinters(complaints, beta=F, gamma=F)
	plot(Comp.hw1)
	Comp.hw1		#a = smoothed mean = 17.7
	Comp.hw1$SSE		#2502

	#force alpha val
	Comp.hw2 <- HoltWinters(complaints, alpha=0.2, beta=F, gamma=F) 
	Comp.hw2		#a = 18.0
	Comp.hw2$SSE		#2526--a substantial increase in error
	lines(Comp.hw2$fitted[,'xhat'], col=4)

	#3.4.2 Holt-Winters Method
	wine.dat <- read.table(paste(web, 'wine.dat', sep=''), header=T)
	attach(wine.dat)
	sweetw.ts <- ts(sweetw, start=c(1980,1), freq=12)
	plot(sweetw.ts, xlab='Time (months)', ylab='Sales (kL)')
	plot(decompose(sweetw.ts))
	sweetw.hw <- HoltWinters(sweetw.ts, seasonal='mult')
	sweetw.hw
	sweetw.hw$coef
	sweetw.hw$SSE	# 477,693.9

	sqrt(sweetw.hw$SSE / length(sweetw))		# 50.5
	sd(sweetw)	#121
	plot(sweetw.hw$fitted)
	plot(sweetw.hw)

	#3.4.3 Four-Year Ahead Forecasts for the Air Passenger Data
	AP.hw <- HoltWinters(AP, seasonal="mult")
	plot(AP.hw)
	AP.predict <- predict(AP.hw, n.ahead = 4*12)
	ts.plot(AP, AP.predict, col=1:2)












# Playing with S&P data
plot(sp)
sp.daily.vals <- (as.vector(sp[!is.na(sp)]))
n <- length(sp.daily.vals)
sp.daily.change <- sp.daily.vals[2:n] / sp.daily.vals[1:(n - 1)]
plot(sp, xlim=c(1991, 2019), ylim=c(0, 5000))
iters <- 1000
n.forecast <- 1000
lastDate <- attr(sp, 'tsp')[2]
dates <- seq(lastDate, lastDate + 4, length=n.forecast)

fillNA <- function(x) {
	nas <- which(is.na(x))
	while(length(nas) > 0) {
		x[nas] <- x[nas - 1]
		nas <- which(is.na(x))
	}
	
	return(x)
}

spFill <- fillNA(sp)

M <- matrix(NA, nrow=n.forecast, ncol=iters)
for (i in 1:iters) {
	sp.forecast <- numeric(n.forecast)
	daily.changes <- sample(sp.daily.change, n.forecast, T)
	sp.forecast[1] <- sp.daily.vals[n] * daily.changes[1]
	for (p in 2:n.forecast) {
		sp.forecast[p] <- sp.forecast[p - 1] * daily.changes[p]
	}
	lines(sp.forecast ~ dates, col=rgb(0,0,1,0.02))
	M[, i] <- sp.forecast
}
qs <- apply(M, 1, quantile, probs=c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
lines(qs[1,] ~ dates, col=1, lty=1)
lines(qs[4,] ~ dates, col=1, lty=1)
lines(qs[7,] ~ dates, col=1, lty=1)

lines(qs[2,] ~ dates, col=2, lty=3)
lines(qs[6,] ~ dates, col=2, lty=3)

lines(qs[3,] ~ dates, col=2, lty=2)
lines(qs[5,] ~ dates, col=2, lty=2)


par(mfrow=c(1,1))

decAdd <- decompose(spFill)
trendAdd <- decAdd$trend
seasAdd <- decAdd$seasonal
eAdd <- decAdd$random

decMult <- decompose(spFill, type='mult')
trendMult <- decMult$trend
seasMult <- decMult$seasonal
eMult <- decMult$random

sd(eAdd, na.rm=T)	# 45.20
sd(eMult, na.rm=T)	# 0.041

plot(decompose(spFill))
plot(decompose(spFill, type='multiplicative'))

par(mfrow=c(2, 1))
plot(sp)
lines(trendAdd + seasAdd, col=2)
plot(sp)
lines(trendMult + seasMult, col=2)

plot(sp, xlim=c(2010, 2016))
lines(trendAdd + seasAdd, col=2)
plot(sp, xlim=c(2010, 2016))
lines(trendMult + seasMult, col=2)

sp.hw <- HoltWinters(sp.daily.vals, beta=F, gamma=F) 
plot(sp.hw)
plot(sp.hw, xlim=c(6100, 6160), ylim=c(2050, 2150))
sp.hw	# smoothing param = 0.93 or almost 1, meaning  it basically just 	
		# predicts that tomorrows val = today's value... of no use
		
SP.hw <- HoltWinters(ts(sp.daily.vals, frequency=250), seasonal="mult")
plot(SP.hw)
plot(SP.hw, xlim=c(25, 27))
SP.predict <- predict(SP.hw, n.ahead = 1*250)
ts.plot(ts(sp.daily.vals, frequency=250), SP.predict, col=1:2)
ts.plot( ts(sp.daily.vals, frequency=250), SP.predict, col=1:2, 
		 xlim=c(25.5, 26.75), ylim=c(1500, 2500) )



spNoNA <- ts(sp[!is.na(sp)])
sp2011NoNA <- ts(sp2011[!is.na(sp2011)])
lodeNoNA <- ts(lode[!is.na(lode)])
acf(spNoNA)
acf(diff(spNoNA))
acf(decompose(spFill)$random[183:8731])
acf(diff(decompose(spFill)$random[183:8731]))

ts.plot(sp2011, lode*350, col=1:2)

acf(cbind(diff(sp2011NoNA), diff(lodeNoNA)))









save.image(file="~/Desktop/R/Time Series/TimeSeries.RData")
detach(Motor.dat)
detach(wine.dat)