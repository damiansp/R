#################################
#                               #
#  Ch 3 Forecasting Strategies  #
#           Exercises           #
#                               #
#################################

load("TimeSeries.RData")
options(digits = 3)

#1
	#a
w <- 1:100
x <- w + 1*rnorm(100)	#k=1
y <- w + 1*rnorm(100)

p <- w + 10*rnorm(100)	#k=10
q <- w + 10*rnorm(100)

u <- w + 100*rnorm(100)	#k=100
v <- w + 100*rnorm(100)

par(mfrow=c(2,3))
plot(x,y); plot(p,q); plot(u,v)
ccf(x,y); ccf(p,q); ccf(u,v)
	
	#b
Time <- 1:370
x <- sin(2*pi*Time / 37)
y <- sin(2*pi*(Time + 4) / 37)

par(mfrow=c(2,2))
plot(x, type='l')
plot(y, type='l')
plot(x, y)
ccf(x,y)



#6
	#a
par(mfrow=c(1,1))
plot(wine.dat$sweetw, type='l')
sw.wine <- ts(wine.dat$sweetw, start=c(1980,1), freq=12)
sw.wine.hw <- HoltWinters(sw.wine, alpha=0.2, beta=0.2, gamma=0.2, seasonal='mult')
sw.wine.hw$SSE	#651777	(alpha, beta, gamma set to 0.2)
sweetw.hw <- HoltWinters(sweetw.ts, seasonal='mult')
sweetw.hw$SSE	#477694 (parameters optimized)
par(mfrow=c(2,1))
plot(sw.wine.hw)
plot(sweetw.hw)
	
	#b
plot(log(sweetw.ts))
sweetw.log.hw <- HoltWinters(log(sweetw.ts), seasonal='mult')
sweetw.log.hw$SSE
par(mfrow=c(2,1))
plot(sweetw.hw)
plot(sweetw.log.hw)
	

save.image("TimeSeries.RData")
quit()


#########################################################################################################################################################################################################
