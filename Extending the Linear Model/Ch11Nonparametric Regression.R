#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===================================#
#									#
#	11. Nonparametric Regression	#
#									#
#===================================#
rm(list = ls())
library(faraway)
library(sm)
library(splines)
library(wavethresh)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')


# Load in example data, and plot (known) function that generated the data for 
# exa and exb
data(exa)
plot(y ~ x, exa, main = 'Example A')
lines(m ~ x, exa)	#actual function

data(exb)
plot(y ~ x, exb, main = 'Example B')
lines(m ~ x, exb)	#actual function

data(faithful)
plot(waiting ~ eruptions, faithful, main = 'Old Faithful')

# 11.1 Kernel Estimators
plot(waiting ~ eruptions, faithful, 
	 main = 'Kernel Estimators with Difft Bandwiths')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 0.1), 
	  col = 'red')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 0.5), 
	  col = 'blue')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 2), 
	  col = 'green')
legend('topleft', legend = c(0.1, 0.5, 2), lty = 1, 
	   col = c('red', 'blue', 'green'), title = 'Bandwidth')

# write a cross-validation function
cv = function(lambda, x, y, kern = 'normal') {
	n = length(x)
	s = 0
	
	for (j in 2:(n - 1)) {
		ks = ksmooth(x[-j], y[-j], kern, lambda)
		closestx = which(abs(ks$x - x[j]) == min(abs(ks$x - x[j])))
		e = (y[j] - ks$y[closestx])^2
		s = s + e
	}
	
	s / n
}

bestLambda = function(lambdas, x, y, kern = 'normal') {
	cvs = numeric(length(lambdas))
	for (i in 1:length(lambdas)) {
		cvs[i] = cv(lambdas[i], x, y)
	}
	
	cbind(lambdas, cvs)
}

bestLambda(lambdas = seq(0.5, 2.1, 0.2), faithful$eruptions, 
		   faithful$waiting) # best is at lambda = 0.7

plot(waiting ~ eruptions, faithful, 
	 main = 'Kernel Estimators with Difft Bandwiths')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 0.1), 
	  col = 'red')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 0.5), 
	  col = 'blue')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 2), 
	  col = 'green')
lines(ksmooth(faithful$eruptions, faithful$waiting, 'normal', 0.7),
	  lwd = 3)
legend('topleft', legend = c(0.1, 0.5, '0.7 (optimal)', 2), lty = 1, 
	   col = c('red', 'blue', 'black', 'green'), title = 'Bandwidth', 
	   lwd = c(1, 1, 3, 1), bty = 'n')

# Try another example
x = rpois(500, 5) + rnorm(500)
y = x^2 * 0.7^x 
y = y + rnorm(500, sd = 0.4)
bestLambda(seq(0.1, 1.5, 0.1), x, y)

plot(y ~ x)
# Drop-one estimators
for (i in 1:300) {
	lines(ksmooth(x[-i], y[-i], 'normal', 1.0), col = rgb(0, 0, 0, 0.2))
}
xv = seq(min(x), max(x), length = 300)
yv = xv^2 * 0.7^xv
lines(xv, yv, col = 2)

plot(y ~ x)
for (lambda in seq(0.6, 4, 0.1)) {
	lines(ksmooth(x, y, 'normal', lambda))
}
lines(xv, yv, col = 2)
lines(ksmooth(x, y, 'normal', 0.7), col = 5)

# The sm library has built in cross-validation functions
hm = hcv(faithful$eruptions, faithful$waiting, display = 'lines')
sm.regression(faithful$eruptions, faithful$waiting, h = hm, 
			  xlab = 'duration', ylab = 'waiting')

hm = hcv(exa$x, exa$y, display = 'lines')
sm.regression(exa$x, exa$y, h = hm)

hm = hcv(exb$x, exb$y, display = 'lines') # best at min. h value
hm = hcv(exb$x, exb$y, display = 'lines', hstart = 0.0018)
sm.regression(exb$x, exb$y, h = hm)
sm.regression(exb$x, exb$y, h = 0.005)

# 11.2 Splines
plot(waiting ~ eruptions, faithful)
lines(smooth.spline(faithful$eruptions, faithful$waiting), col = 2)

plot(y ~ x, exa)
lines(exa$x, exa$m)
lines(smooth.spline(exa$x, exa$y), col = 'red')

plot(y ~ x, exb)
lines(exb$x, exb$m)
lines(smooth.spline(exb$x, exb$y), col = 2)

# Regression Splines
# fits local linear with evenly spaced knots
rhs = function(x, c) { ifelse(x > c, x - c, 0) }
curve(rhs(x, 0.5), 0, 1) 
(knots = 0:9 / 10 )
dm = outer(exa$x, knots, rhs)
matplot(exa$x, dm, type = 'l', lty = 1)

g = lm(exa$y ~ dm)
plot(y ~ x, exa)
lines(exa$x, predict(g))

# make knots denser in area of greater curavature:
newknots = c(0, .5, .6, .65, .7, .75, .8, .85, .9, .95)
dmn = outer(exa$x, newknots, rhs)
matplot(exa$x, dmn, type = 'l')
gn = lm(exa$y ~ dmn)
plot(y ~ x, exa)
lines(exa$x, predict(g), col = 2)
lines(exa$x, predict(gn), col = 4)

matplot(bs(seq(0, 1, length = 1000), df = 12), type = 'l', lty = 1)
sm1 = lm(y ~ bs(x, 12), exa)
plot(y ~ x, exa)
lines(m ~ x, exa, col = 1)
lines(predict(sm1) ~ x, exa, col = 2)


# Place n knots in arbitrary locations and determine SSE, repeat iters times
# and return the set of knots with the lowest SSE
bestKnots = function(x, y, nKnots, iters, alpha = 0.01) {
	plot(y ~ x)
	
	# Keep track of the best least squares estimate, and the knots that gave 
	# the estimate
	bestLS = Inf
	bestKnots = numeric(nKnots)
	bestMod = lm(y ~ 1)
	
	for (i in 1:iters) {
		knots = sort(runif(nKnots, min(x), max(x)))
		dm = outer(x, knots, rhs)
		g = lm(y ~ dm)
		lines(x, predict(g), col = rgb(0, 0, 0, alpha))
		sse = sum((predict(g) - y)^2)

		if (sse < bestLS) {
			bestLS = sse
			bestKnots = knots
			bestMod = g
		}
	}
	
	lines(x, predict(g), col = 2)
	legend('topleft', legend = paste('SSE:', round(bestLS, 4)))
	
	return (list(bestKnots, bestLS))
}

(best = bestKnots(exa$x, exa$y, 10, 10000))

# 11.3 Local Polynomials
plot(waiting ~ eruptions, faithful)
f = loess(waiting ~ eruptions, faithful)
i = order(faithful$eruptions)
lines(f$x[i], f$fitted[i], col = 2)

plot(y ~ x, exa)
lines(exa$x, exa$m)
f = loess(y ~ x, exa)
lines(f$x, f$fitted, col = 2)
f = loess(y ~ x, exa, span = 0.22)
lines(f$x, f$fitted, col = 4)

plot(y ~ x, exb)
f = loess(y ~ x, exb)
lines(f$x, f$fitted, col = 2)
f = loess(y ~ x, exb, span = 1)
lines(f$x, f$fitted, col = 4)
lines(exb$x, exb$m)



# 11.4 Wavelets
#library(wavethresh)
wds = wd(exa$y, filter.number = 1, bc = 'interval') 
draw(filter.number = 1, family = 'DaubExPhase')	#?
plot(wds)

wtd = threshold(wds, policy = 'manual', value = 9999)
fd = wr(wtd)

plot(y ~ x, exa)
lines(m ~ x, exa)
lines(fd ~ x, exa, col = 2)

wtd2 = threshold(wds)
fd2 = wr(wtd2)
lines(fd2 ~ x, exa, col = 4)

wds = wd(exa$y, filter.number = 2, bc = 'interval')
draw(filter.number = 2, family = 'DaubExPhase')
plot(wds)

wtd = threshold(wds)
fd = wr(wtd)
plot(y ~ x, exa)
lines(m ~ x, exa)
lines(fd ~ x, exa, col = 2)



# 11.5 Other Methods
# Nearest neighbor
nearNeighb = function(x, y, lambda, f) {
	y = y[order(x)]
	x = x[order(x)]
	xMid = x
	
	for (i in 1:length(x)) {
		nearest = order(abs(x - x[i]))[-i][1:lambda]
		xMid[i] = f(y[nearest])
	}
	
	xMid
}

plot(y ~ x, exa)
#lines(m ~ x, exa, col = 4)
l2 = nearNeighb(exa$x, exa$y, lambda = 2, f = mean)
lines(exa$x, l2, col = 2)
l2 = nearNeighb(exa$x, exa$y, lambda = 2, f = median)
lines(exa$x, l2, col = 2)

l4 = nearNeighb(exa$x, exa$y, lambda = 4, f = mean)
lines(exa$x, l4, col = 4, lty = 2)
l4 = nearNeighb(exa$x, exa$y, lambda = 4, f = median)
lines(exa$x, l4, col = 4)

plot(y ~ x, exa)
l8 = nearNeighb(exa$x, exa$y, lambda = 8, f = mean)
lines(exa$x, l8, col = 5, lty = 2)
l8 = nearNeighb(exa$x, exa$y, lambda = 8, f = median)
lines(exa$x, l8, col = 5)

l16 = nearNeighb(exa$x, exa$y, lambda = 16)
lines(exa$x, l16, col = 6)
l32 = nearNeighb(exa$x, exa$y, lambda = 32)
lines(exa$x, l32, col = 8)

plot(y ~ x, exa)
l64 = nearNeighb(exa$x, exa$y, lambda = 64, f = mean)
lines(exa$x, l64, col = 2, lty = 2)
l64 = nearNeighb(exa$x, exa$y, lambda = 64, f = median)
lines(exa$x, l64, col = 2)
lines(m ~ x, exa, col = 4)


# GENERAL NOTE: Generally lo(w)ess lines will be good, robust, all-purpose 
# smoothers, though smoothing splines are more efficient if no outliers are 
# present
# 11.7 Multivariate Predictors
data(savings)
head(savings)
y = savings$sr
x = cbind(savings$pop15, savings$ddpi)
sm.regression(x, y, h = c(1, 1), xlab = 'pop15', ylab = 'growth', 
			  zlab = 'savings rate')
sm.regression(x, y, h = c(5, 5), xlab = 'pop15', ylab = 'growth', 
			  zlab = 'savings rate')






save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')