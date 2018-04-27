#====================#
#                    #
#  Ch 2 Correlation  #
#                    #
#====================#
rm(list=ls())
load('~/Desktop/R/Time Series/TimeSeries.RData')
source('~/Desktop/SM/get.hist.quote2.R', chdir = TRUE)
options(digits = 5)
library(MASS)

sp <- get.hist.quote2( '^gspc', #start='2004-01-01', 
					   quote='AdjClose' )
sp <- ts(as.ts(sp), start=1991, frequency=365)

#data(AirPassengers)


# 2.1 Purpose


# 2.2 Expectation and the Ensemble
	# 2.2.1 Expected value
	Herald.dat <- read.table(
			"http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Herald.dat",
			header=T )
	attach(Herald.dat)

	x <- CO
	y <- Benzoa
	n <- length(x)
	sum((x - mean(x))*(y - mean(y))) / (n - 1)	# 5.51, same as:
	cov(x, y)	# abbreviated form of previous = 5.51

	cov(x, y) / (sd(x)*sd(y))	#0.355, same as:
	cor(x, y)
	
	# 2.2.5 Autocorrelation
	wave.dat <- read.table( 
				 "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/wave.dat", 
				 header=T
				)
	attach(wave.dat)
	plot(ts(waveht))
	plot(ts(waveht[1:60]))
	acf(waveht)$acf	# row values are for lag values k + 1 (row 1: k = 0), 
					# thus for lag 1:
	acf(waveht)$acf[2]
	#autocovariance is found as:
	acf(waveht, type='covariance')$acf



# 2.3 The Correlogram
	# 2.3.1 General discussion
	acf(waveht)
	acf(AirPassengers)

	# 2.3.2 Example based on air passenger series
	AP <- AirPassengers
	AP.decom <- decompose(AP, "multiplicative")
	plot(ts(AP.decom$random))
	abline(h=1, lty=2)
	acf(AP.decom$random[7:138])	#indexing to remove NAs
	#Check the effectiveness of removing trend and seasonal variation:
	sd(AP[7:138])	#109 for all
	sd(AP[7:138] - AP.decom$trend[7:138])	#41.1 after removing trend
	sd(AP.decom$random[7:138])	#and only 0.033 when trend & seasonal removed

	# 2.3.3 Example based on the Font Reservoir series
	Fontdsdt.dat <- read.table( 
			 "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Fontdsdt.dat", 
			 header=T )
	attach(Fontdsdt.dat)
	plot(ts(adflow))
	acf(adflow, xlab='lag(months)', main='')
	


# 2.4 Covariance of sums of random variables

	
	











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
plot(decompose(spFill))

spNoNA <- sp[!is.na(sp)]
acf(spNoNA)
acf(diff(spNoNA))
acf(decompose(spFill)$random[183:8731])
acf(diff(decompose(spFill)$random[183:8731]))








save.image(file="~/Desktop/R/Time Series/TimeSeries.RData")
detach(Herald.dat)
detach(wave.dat)
detach(Fontdsdt.dat)