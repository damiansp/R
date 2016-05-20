#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===========================#
#							#
#	12. Additive Models		#
#							#
#===========================#
rm(list = ls())
#install.packages('mda', repos = 'http://cran.us.r-project.org')
library(faraway)
library(gam)
library(mda)
library(mgcv)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')


# 12.1 Additive Models Using the gam Package
data(ozone)

pairs(ozone, panel = panel.smooth)
olm = lm(O3 ~ temp + ibh + ibt, ozone)
summary(olm)
par(mfrow = c(2, 2))
plot(olm)

truehist(ozone$temp); rug(ozone$temp); lines(density(ozone$temp))
truehist(ozone$ibh); rug(ozone$ibh); lines(density(ozone$ibh))
truehist(ozone$ibt); rug(ozone$ibt); lines(density(ozone$ibt))
par(mfrow = c(1, 1))

amgam = gam(O3 ~ lo(temp) + lo(ibh) + lo(ibt), data = ozone)
summary(amgam)

1 - 5935.1 / 21115.4	# Appx. Rsq = 1 - res.dev/null.dev = 0.72
# but uses more df!
# p values appx and should be treated w/ skepticism; better to leave out 
# predictor of interest first, then compare models w/ F test:

amgamr = gam(O3 ~ lo(temp) + lo(ibh), data = ozone)
summary(amgamr)
anova(amgamr, amgam, test = "F")	# suggests ibt not significant

# To observe the fit:
par(mfrow = c(1, 3))
plot(amgam, residuals = T, se = T, pch=".")	


# 12.2 Additive Models Using mgcv
#library(mgcv)
detach(package:gam)
ammgcv = gam(O3 ~ s(temp) + s(ibh) + s(ibt), data = ozone)
summary(ammgcv)
par(mfrow = c(1, 3))
plot(ammgcv)

# Is the change in trend in temp significant?
am1 = gam(O3 ~ s(temp) + s(ibh), data = ozone)
am2 = gam(O3 ~ temp + s(ibh), data = ozone)
anova(am2, am1, test = "F")	# suggests significicant nonlinear response in temp

# test for interaction between temp:ibh:
amint = gam(O3 ~ s(temp, ibh) + s(ibt), data = ozone)
summary(amint)

# and compare with previous model:
anova(ammgcv, amint, test = "F")
par(mfrow = c(1, 2))
plot(amint)	# note contour lines in 1st plot essentially parallel
par(mfrow = c(1, 1))
vis.gam(amint, theta = -45)	# similarly the contour is consistent over the linear predictor variable, both suggesting no significant interaction; in the case of the ANOVA, the amint (interaction) model is actually the simpler (cf. res.df)

rhs = function(x, c) ifelse(x > c, x - c, 0)
lhs = function(x, c) ifelse(x < c, c - x, 0)

# 60 and 1000 are appx inflection points as seen in plot(ammgcv)
olm2 = lm(O3 ~ rhs(temp, 60) + lhs(temp, 60) + rhs(ibh, 1000) + lhs(ibh, 1000),
		  data = ozone)
summary(olm2)	# allows for linear approximation of the gam, and for easily writing prediction formulae

predict(ammgcv, data.frame(temp = 60, ibh = 2000, ibt = 100), se = T) 
# w/in data range
predict(ammgcv, data.frame(temp = 120, ibh = 2000, ibt = 100), se = T) 
# external to data range

#check usual diagnostics:
par(mfrow = c(1, 1))
plot(predict(ammgcv), residuals(ammgcv), xlab = "Predicted", ylab = "Residuals")
# nonconstant variance
qqnorm(resid(ammgcv)); qqline(resid(ammgcv))	# ok

# Fuller model:
amred = gam(O3 ~ s(vh) + s(wind) + s(humidity) + s(temp) + s(dpg) + s(vis) + 
			s(doy), data = ozone)
summary(amred)

# campare with lm with insignif terms removed
alm = lm(O3 ~ vis + doy + ibt + humidity + temp, data = ozone)
summary(alm)



# 12.3 Generalized Additive Models
gammgcv = gam(O3 ~ s(temp) + s(ibh) + s(ibt), family = poisson, scale = -1, 
			  data = ozone)	
# scale=-1: negative vals for this parameter indicate that dispersion should be 
# estimated rather than fixed (if not specified dispersion assumed to be one)
summary(gammgcv)
par(mfrow = c(1, 3))
plot(gammgcv)



# 12.4 Alternating Conditional Expectations
x = ozone[, c("temp", "ibh", "ibt")]
library(acepack)

acefit = ace(x, ozone$O3)	#NOTE syntax: ace(X, y); X as data.frame
summary(acefit)	# not particularly helpful, so:
summary(lm(acefit$ty ~ acefit$tx))	#transformed x and y, presumable

par(mfrow = c(1, 1))
library(MASS)
truehist(ozone$O3); rug(jitter(ozone$O3)); lines(density(ozone$O3))
par(mfrow = c(2, 2))
plot(ozone$O3, acefit$ty, xlab = expression(O[3]), ylab = expression(theta(O[3])))
plot((x[,1]), (acefit$tx[,1]), xlab = "temp", ylab = "f(temp)")
plot((x[,2]), (acefit$tx[,2]), xlab = "ibh", ylab = "f(ibh)")
plot((x[,3]), (acefit$tx[,3]), xlab = "ibt", ylab = "f(ibt)")	
# transformation likely overfitting

# Now for the full data set
x = ozone[, -1]
acefit = ace(x, ozone$O3)
summary(lm(acefit$ty ~ acefit$tx))
par(mfrow = c(3, 3))
plot((x[,1]), (acefit$tx[,1]), xlab = 'vh')
plot((x[,2]), (acefit$tx[,2]), xlab = 'wind')
plot((x[,3]), (acefit$tx[,3]), xlab = 'humidity')
plot((x[,4]), (acefit$tx[,4]), xlab = 'temp')
plot((x[,5]), (acefit$tx[,5]), xlab = 'ibh')
plot((x[,6]), (acefit$tx[,6]), xlab = 'dpg')
plot((x[,7]), (acefit$tx[,7]), xlab = 'ibt')
plot((x[,8]), (acefit$tx[,8]), xlab = 'vis')
plot((x[,9]), (acefit$tx[,9]), xlab = 'doy')
# NOTE: in genearl it is dangerous to use this method for final models due to the 
# overfitting tendency, but is useful to discover transformations which may then be 
# applied to parametric models.


y = cbind(ozone$O3, ozone$O3^2, sqrt(ozone$O3))
x = ozone[, c("temp", "ibh", "ibt")]
cancor(x, y)	
# c or between xs and ys = 0.83 here (= r, so Rsq = 0.83^2 = 0.69, not particularly 
# competitive)



# 12.5 Additivity and Variance Stabilization (AVAS)
# purpose of AVAS to obtain additivity and variance stabilization, NOT necessarily to produce the best fit
avasfit = avas(x, ozone$O3)
# plot transformations:
par(mfrow = c(2, 2))
plot(ozone$O3, avasfit$ty, xlab = expression(O[3]), ylab = expression(theta(O[3])))
plot(x[,1], avasfit$tx[,1], xlab = "temp", ylab = "f(temp)")
plot(x[,2], avasfit$tx[,2], xlab = "ibh", ylab = "f(ibh)")
plot(x[,3], avasfit$tx[,3], xlab = "ibt", ylab = "f(ibt)")

# See if transformation of responses matches some convenient functional form
i = order(ozone$O3)
par(mfrow = c(1, 1))
plot(ozone$O3[i], avasfit$ty[i], type = "l", xlab = "O3", 
	 ylab = expression(theta(O3)))
gs = lm(avasfit$ty[i] ~ sqrt(ozone$O3[i]))
lines(ozone$O3[i], gs$fit, col = 'red')
gl = lm(avasfit$ty[i] ~ log(ozone$O3[i]))
lines(ozone$O3[i], gl$fit, col = "blue")	
# neither works very well over the full range of data
lmod = lm(avasfit$ty ~ avasfit$tx)
summary(lmod) # fit not so good, but look at diagnostics:
par(mfrow = c(2, 2))
plot(lmod)	# amazing!



# 12.6 Generalized Additive Mixed Models (GAMM)
data(epilepsy)
egamm = gamm(seizures ~ treat * expind + s(age), family = poisson,
		 	 random = list(id = ~1), data = epilepsy, subset = (id != 49))
summary(egamm$gam)



# 12.7 Multivariate Adaptive Regression Splines (MARS)
#library(mda)
data(ozone)
a = mars(ozone[, -1], ozone[, 1])
# Use basis functions as predictors in a lm
summary(lm(ozone[, 1] ~ a$x[, -1]))

# Reduce model size to compare with previous additive models
a = mars(ozone[,-1], ozone[,1], nk = 7)	# nk term limits no. of model terms
summary(lm(ozone[,1] ~ a$x[, -1]))

# Now allow 2-way interaction terms:
a = mars(ozone[,-1], ozone[,1], nk = 10, degree = 2)
summary(lm(ozone[, 1] ~ a$x[, -1]))

# Examine the form of the basis functions
a$factor[a$selected.terms, ]	
# first row is intercept (all 0); 1 indicates a 'right hockey stick' and -1 a 'left'

# To predict the effect of doy and ibh, e.g., requires plotting the transformation
# as a function of the predictor:
# For ibh (non-0 in a$x[ ,4])
plot(ozone[, 'ibh'], a$x[, 4]*a$coef[4], xlab = "ibh", ylab = "Contribution of ibh")
# And for doy (a$x[, 7])
plot(ozone[, 'doy'], a$x[, 5]*a$coef[5] + a$x[, 6]*a$coef[6], xlab = "doy", 
	 ylab = "Contribution of doy")
	 
# For temp and humidity there is an interaction (row 7 has 1 or -1 in both)
humidity = seq(10, 100, len = 20)	# range over which humidity varies (w/ temp)
temp = seq(20, 100, len = 20)
medians = apply(ozone, 2, median)

# create data using median vals for all other predictors, and all combinations for 
# temp and humidity
pdf = matrix(medians, nrow = 400, ncol = 10, byrow = T)
pdf[, 4] = rep(humidity, 20)
pdf[, 5] = rep(temp, rep(20, 20))
pdf = as.data.frame(pdf)
names(pdf) = names(medians)
head(pdf)
# Use to predict and plot
z = predict(a, pdf[, -1])	
zm = matrix(z, ncol = 20, nrow = 20)
contour(humidity, temp, zm, xlab = 'Humidity', ylab = 'Temperature')
persp(humidity, temp, zm, xlab = 'Humidity', ylab = 'Temperature', zlab = 'Ozone',
	  theta = -40)

# And check model diagnostics
qqnorm(a$res, main = '')
qqline(a$res)

plot(a$fit, a$res, xlab = 'Fitted', ylab = 'Residuals')
abline(h = 0)
lines(lowess(a$fit, a$res), col = 2)




save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')
