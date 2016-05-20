#===============================#
#								#
#  Nonlinear Regression with R  #
#								#
#===============================#

rm(list=ls())
library(drc)
library(lattice)
library(MASS)
library(NISTnls)
library(nlrwr)

load('~/Desktop/R/NonlinearRegression/nlr.RData')
data(M.merluccius)
data(RScompetition)
data(S.alba)
data(L.minor)
data(segreg)
data(vapCO)

# 1.1 A Stock Recruitment Model

pairs(M.merluccius)
plot(num.fish ~ spawn.biomass, data=M.merluccius, xlab='Spawning biomass (1000 tons)', 
	 ylab="Recruitment (millions of fish)")
	 
# 1.2 Competition between Plant Biotypes

# 1.3 Grouped Dose-Response Data
plot(biomass ~ x, data=RScompetition, log='', xlab=Density ~ (plants/m^2), 
	 ylab=Biomass ~ of ~ sensitive ~ biotype ~ (g/plant), 
	 pch=as.numeric(as.factor(RScompetition$z)))

xyplot(DryMatter ~ Dose | Herbicide, data=S.alba, scales=list(x = list(log=T)), 
	   ylab="Dry matter (g/pot)", xlab='Dose (g/ha)')
	   


#=====================#
#					  #
#  2 Getting Started  #
#					  #
#=====================#

# 2.2 Getting Started with nls()
pairs(L.minor)

	# 2.2.2 Model Fitting
	plot(rate ~ conc, data=L.minor, ylab='Uptake rate (weight/h)', 
		 xlab = Substrate ~ concentration ~ (mmol ~ m^-3), ylim=c(0, 120))
	L.minor.m1 <- nls(rate ~ (Vm * conc) / (K + conc), data=L.minor, start=list(K=110, Vm=75), 
					  trace=T)	# if trace = T, parameter estimates are displayed at ea iter
	deviance(L.minor.m1) # RSS: use to compare competing models
	logLik(L.minor.m1)
	coef(L.minor.m1)
	summary(L.minor.m1)
	xv <- seq(0, 200, 0.5)
	yv <- (17.1 * xv) / (126 + xv)
	lines(yv ~ xv, col=2)
	
	fitted(L.minor.m1)
	
	concVal <- with(L.minor, seq(0, max(conc), length.out=100))
	predict(L.minor.m1, data.frame(conc=concVal))
	plot(rate ~ conc, data=L.minor, ylim=c(0, 130))
	lines(L.minor$conc, fitted(L.minor.m1), col=2)
	plot(rate ~ conc, data = L.minor, ylim=c(0, 130))
	lines(concVal, predict(L.minor.m1, newdata=data.frame(conc=concVal)), col=2)
	abline(h=coef(L.minor.m1)[2], lty=2)

	# RSS as a function of different param vals
	L.minor.m1con <- nlsContourRSS(L.minor.m1)	
	plot(L.minor.m1con, nlev=20, col.pal=heat.colors(1000))



# 2.3 Generalized Linear Models
L.minor.m4 <- glm(rate ~ I(1 / conc), data=L.minor, family=stats::gaussian('inverse'))
summary(L.minor.m4)





#=======================================#
#										#
#  3 Starting Values and Self-Starters  #
#										#
#=======================================#

# 3.1 Finding Starting Values
	# 3.1.1 Graphical Exploration
	plot(y ~ x, data=Chwirut2, xlab='metal dist', ylab='ultrasonic response', ylim=c(0, 100))
	
	# Define expected function
	expFct <- function(x, b1, b2, b3) {
		exp(-b1*x) / (b2 + b3*x)
	}
	
	curve(expFct(x, b1=1, b2=0.01, b3=1), add=T, col=2)			# red
	curve(expFct(x, b1=0.1, b2=0.01, b3=1), add=T, col=3)		# green
	curve(expFct(x, b1=0.1, b2=0.01, b3=0.1), add=T, col=4)		# blue
	curve(expFct(x, b1=0.1, b2=0.01, b3=0.01), add=T, col=5)	# cyan (best)
	curve(expFct(x, b1=0.02, b2=0.01, b3=0.01), add=T, col=6)	# magenta
	
	Chwirut2.m1 <- nls(y ~ expFct(x, b1, b2, b3), data=Chwirut2, 
					   start=list(b1=0.1, b2=0.01, b3=0.01), trace=T)
	summary(Chwirut2.m1)
	curve(expFct(x, b1=0.1666, b2=0.0052, b3=0.0122), add=T)	# black
	
	
	# 3.1.2 Searching a Grid
	grid.Chwirut2 <- expand.grid(list(b1=seq(0.1, 1, 0.1), b2=0.01, b3=seq(0.1, 1, 0.1)))
	Chwirut2.m2a <- nls2(y ~ expFct(x, b1, b2, b3), data=Chwirut2, start=grid.Chwirut2, 
						 algorithm='brute-force', trace=T)
	summary(Chwirut2.m2a)
	

# 3.2 Using Self-Starter Functions
	# 3.2.1 Built-In Self-Starter Functions for nls()
	L.minor.m2 <- nls(rate ~ SSmicmen(conc, Vm, K), data=L.minor)
	summary(L.minor.m2)
	
	
	# 3.2.2 Defining a Self-Starter Function for nls()
	# Example uses function: f(x, (b, y[0])) = y[0]exp(x/b) (radioactive decay)
	# y[0] = initial amount of substance at time x = 0; decay rate goverened by b
	# log transform: log(y[0]) + (1/b)*x (a linear model in x)
	# Thus, transforming back: y[0] = exp(beta[0]) -intercept- and b = 1/beta[1] -slope-
	expModel <- function(predictor, b, y0) {
		y0 * exp(predictor / b)
	}
	
	# define initial value routine:
	# @param mCall: the nls() call where all the specified args are specified by full name
	# @param LHS: left-hand side in the model formula in the nls() call (the response)
	# @param data: the data.frame used in the nls() 
	expModelInit <- function(mCall, LHS, data) {
		# sort predictor and response values by increasing predictor vals
		xy <- sortedXyData(mCall[['predictor']], LHS, data)
		lmFit <- lm(log(xy[, 'y']) ~ xy[, 'x'])
		coefs <- coef(lmFit)
		y0 <- exp(coefs[1])
		b <- 1 / coefs[2]
		value <- c(b, y0)
		names(value) <- mCall[c('b', 'y0')]
		value
	}
	
	# self-starter function:
	SSexp <- selfStart(expModel, expModelInit, c('b', 'y0'))
	
	with(RGRcurve, SSexp(Day, 4, 0.2))
	getInitial(RGR ~ SSexp(Day, b, y0), data=RGRcurve) # problem....
	RGRcurve.m1 <- nls(RGR ~ SSexp(Day, b, y0), data=RGRcurve)
	summary(RGRcurve.m1)
	


#===================#
#					#
#  4 More on nls()  #
#					#
#===================#

# 4.2 Supplying Gradient Information
	# 4.2.1 Manual Supply
	# partial derivatives fro the Michaelis-Menten model:
		# ∂f/∂K = -V[m]x / (K + x)^2
		# ∂f/∂V[m] = x / (K + x)
	MMfct1 <- function(conc, K, Vm) {
		numer <- Vm * conc
		denom <- K + conc
		mean <- numer / denom
		partialK <- -numer / denom^2
		partialVm <- mean / Vm
		attr(mean, 'gradient') <- cbind(partialK, partialVm)
		return(mean)
	}
	
	L.minor.mgr1 <- nls(rate ~ MMfct1(conc, K, Vm), data=L.minor, start=list(K=20, Vm=120))
	summary(L.minor.mgr1)
	
	
	# 4.2.2 Automatic Supply
	MMfct2 <- deriv(~Vm * conc / (K + conc), c('K', 'Vm'), function(conc, K, Vm) {} )
	L.minor.mgr2 <- nls(rate ~ MMfct2(conc, K, Vm), data=L.minor, start=list(K=20, Vm=120))
	summary(L.minor.mgr2)
	
# 4.3 Conditionally Linear Parmeters
# Ex. Michaelis-Menten model: V[m]x / (K + x) can be rewritten as:
# (x / (K + x)) * V[m]  or coef * linear term; hence V[m] is conditionally linear (K is not)
	# 4.3.1 nls() Using the 'plinear' Algorithm
	L.minor.m3 <- nls(rate ~ conc / (K + conc), data=L.minor, algorithm='plinear', 
					  start=list(K=20))
	summary(L.minor.m3)
	
	
	# 4.3.2 A Pedestrian Approach
	plot(C ~ Temp, data = segreg)

	profRSS1 <- function(gamma) {
		deviance(lm(C ~ pmax(0, Temp - gamma), data=segreg))
	}
	
	profRSS2 <- Vectorize(profRSS1, 'gamma')
	plot(profRSS2(Temp) ~ Temp, data=segreg, type='l', xlab=expression(gamma), 
		 ylab="Profile RSS")	# gammma est at min(RSS) = appx. 43
	

# 4.4 Fitting Models with Several Predictor Variables
	# 4.4.1 Two-Dimensional Predictor
	# Using: f(x, z, (a, b, c)) = a / (1 + b*(x + c*z))
	RScomp.m1 <- nls(biomass ~ a / (1 + b*(x + c*z)), data=RScompetition, 
					 start=list(a=20, b=1, c=1))
	summary(RScomp.m1)
	virDensity <- with(RScompetition, x + coef(RScomp.m1)[3]*z)
	virDenVal <- seq(0, max(virDensity), length.out=100)
	biomassVal <- predict(RScomp.m1, data.frame(x=virDenVal, z=0))
	plot(biomassVal ~ virDenVal, type='l', ylab="Biomass of sensitive biotype (g/plant)",
		 xlab=Virtual ~ density ~ (plants/m^2))
	with(RScompetition, points(biomass ~ virDensity, col=2))
	
	# 4.4.2 General Least-Squares Minimization
	# Demodulator formula: 0 = (Ii - I0)^2 -2gamma*sin(phi)(Ii - I0)(Qi - Q0) + 
	#                          gamma^2(Qi - Q0)^2 - (rho*gamma*cos(phi))^2
	IQsig.m1 <- nls(~((I - I0)^2 - 2*gamma*sin(phi)*(I - I0)*(Q - Q0) + 
					 gamma^2 * (Q - Q0)^2) - (rho*gamma*cos(phi))^2, data=IQsig,
					start=list(I0=-0.005, gamma=1, phi=-0.005, Q0=-0.005, rho=1) )
	plot(Q ~ I, data=IQsig)
	theta <- 0:360 * (pi/180)
	lines(cos(theta), sin(theta))
	summary(IQsig.m1)
	

# 4.5 Error Messages


# 4.6 Controlling nls()
nls.control() # shows default vals; may be amended



#=======================#
#						#
#  5 Model Diagnostics  #
#						#
#=======================#

# 5.1 Model Assumptions
# 5.2 Checking the Mean Structure
	# 5.2.1 Plot of the Fitted Regression Curve
	# model for next plot: press = f(temp, (A, B, C)) = A - B / (C + temp)
	plot(p ~ T, data=vapCO, log='y', xlab='Temp (K)', ylab='Press (Pa)')
	
	vapCO.m1 <- nls(log(p) ~ A - B/(C + T), data=vapCO, start=list(A=10, B=100, C=-10))
	lines(vapCO$T, exp(fitted(vapCO.m1)))
	
	# next mod: f(x, (b, d, e)) = d / (1 + (x/e)^b)
	plot(weight ~ conc, data=lettuce, xlab='Concentration (mg/L)', ylab='Biomass (g)', 
		 log='x')
	
	# 5.2.2 Residual Plots
	# 5.2.3 Lack-of-Fit Tests
	plot(fitted(vapCO.m1), resid(vapCO.m1), xlab='Fitted Values', ylab='Residuals')
	abline(h=0, lty=2)
	plot(vapCO.m1)
	
	plot(rootl ~ conc, data=ryegrass, xlab='Concentration (mM)', ylab='Root length (cm)')
	ryegrass.m1 <- lm(rootl ~ as.factor(conc), data=ryegrass) # basic ANOVA model
	ryegrass.m2 <- nls(rootl ~ c + (d - c)/(1 + exp(b*(log(conc) - log(e)))), 
					   start=list(b=1, c=0.6, d=8, e=3), data=ryegrass)	
	anova(ryegrass.m2, ryegrass.m1) # large p value indicates support for the nl model
	summary(ryegrass.m1)
	summary(ryegrass.m2)
	
	# Likelihood Ratio Test
	Q <- -2 * (logLik(ryegrass.m2) - logLik(ryegrass.m1)) # submodel - more general model
	dfQ <- df.residual(ryegrass.m2) - df.residual(ryegrass.m1)
	1 - pchisq(Q, dfQ)	# large p value indivates support for the nl model
	

# 5.3 Variance Homogeneity
	# 5.3.1 Absolute Residuals
	# 5.3.2 Levene's Test
	# (requires replicates)
	with(ryegrass, leveneTest(rootl, as.factor(conc))) # non-significant p value indicates	
													   # assumption of homogeneity of 
													   # variance is satisfied

# 5.4 Normal Distribution
	# 5.4.1 QQ Plot
	standardRes <- resid(ryegrass.m2) / summary(ryegrass.m2)$sigma
	qqnorm(standardRes)
	abline(0, 1)
	shapiro.test(standardRes)


# 5.5 Independence
plot(resid(vapCO.m1), c(resid(vapCO.m1)[-1], NA), xlab='Residuals', ylab='Lagged residuals')



#===================================#
#									#
#  6 Remedies for Model Violations  #
#									#
#===================================#

# 6.1 Variance Modeling
	# 6.1.1 Power-of-the-Mean Variance Model
	plot(RGR ~ Day, data=RGRcurve, xlab='Time (days)', ylab='Relative growth rate (%)')
	# model variance as:
	# var(e[i]) = sig^2(f(x[i], beta))^(2*theta)
	RGRcurve.m2 <- gnls(RGR ~ SSexp(Day, a, b), data=RGRcurve, weights=varPower())
	summary(RGRcurve.m2)
	
# 6.2 Transformations
	# 6.2.1 Tranform-Both-Sides Approach
	# model: f(S, (alpha, k)) = alpha*S*exp(-k*S)
	plot(recruits ~ spawners, data=sockeye[-12, ], xlab='Spawners (1000)', 
		 ylab='Recruits (1000)')
		 
	sockeye.m1 <- nls(recruits ~ beta1 * spawners * exp(-beta2 * spawners), 
					  data=sockeye[-12, ], start=list(beta1=2, beta2=0.001))
	plot(sockeye.m1)
	plot(fitted(sockeye.m1), abs(residuals(sockeye.m1)), xlab='fitted vals', ylab='|resids|')
	
	sockeye.m2 <- boxcox.nls(sockeye.m1)
	bcSummary(sockeye.m2)
	
	summary(sockeye.m1)
	summary(sockeye.m2)

# 6.3 Sandwich Estimators
vcov(sockeye.m1)
sandwich(sockeye.m1)
summary(sockeye.m1)
# just the coef summary:
coeftest(sockeye.m1)
coeftest(sockeye.m1, vcov=sandwich)

# 6.4 Weighting
	# 6.4.1 Decline in Nitrogen Content in Soil
	exp1
	# Model: 
	# N content = f(time, (a1, a2, b1, b2)) = a1*exp(-exp(a2)*time) + b1*exp(-exp(b2)*time)
	plot(Nremaining ~ time, data=exp1, xlab='Time (yrs)', ylab='N content (%)', 
		 ylim=c(0, 100))
	
	exp1.m1 <- nls(Nremaining ~ SSbiexp(time, a1, a2, b1, b2), data=exp1)
	# Weight according to sd among replicates:
	exp1.m2 <- nls(Nremaining ~ SSbiexp(time, a1, a2, b1, b2), data=exp1,
				   weights=norep/(stdev*stdev))
	weights(exp1.m2)
	summary(exp1.m1)
	summary(exp1.m2)
	
	timeVal <- with(exp1, seq(min(0), max(time), length.out=150))
	lines(timeVal, predict(exp1.m1, newdata=data.frame(time=timeVal)), col=2)
	lines(timeVal, predict(exp1.m2, newdata=data.frame(time=timeVal)), col=4)



#======================================#
#									   #
#  7 Uncertainty, Hypothesis Testing,  #
#          and Model Selection         #
#									   #
#======================================#

# 7.1 Profile Likelihood
L.minor.m1 <- update(L.minor.m1, trace=F)
L.minor.m1pro <- profile(L.minor.m1)
plot(L.minor.m1pro, conf=c(0.95, 0.99))
plot(L.minor.m1pro, conf=c(0.95, 0.99), absVal=F)
confint(L.minor.m1, level=0.99)


save.image('~/Desktop/R/NonlinearRegression/nlr.RData')