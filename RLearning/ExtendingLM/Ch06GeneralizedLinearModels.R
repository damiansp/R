#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===================================#
#									#
#	6. Generalized Linear Models	#
#									#
#===================================#
rm(list = ls())
library(faraway)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')



# 6.2 Fitting a GLM
data(bliss)
modl = glm(cbind(dead, alive) ~ conc, family = binomial, bliss)
summary(modl)

# For binomial response:
# 	eta = log(mu / (1 - mu))	== logit(mu)
#	deta / dmu = 1 / (mu * (1 - mu))
#	V(mu) = (mu * (1 - mu)) / n
#	w = n * mu * (1 - mu)
#	and y is the proportion, not the count

y = bliss$dead / 30	# convert to fraction of total
mu = y	#initial est of mu
eta = logit(mu)
z = eta + (y - mu) / (mu * (1 - mu))
w = 30 * mu * (1-mu)
lmod = lm(z ~ conc, weights = w, bliss)
summary(lmod)	# equal to model with initial estimates before converging

#five iterations toward convergence:
for(i in 1:5){
	eta = lmod$fit
	mu = ilogit(eta)
	z = eta + (y - mu) / (mu * (1 - mu))
	w = 30 * mu * (1 - mu)
	lmod = lm(z ~ bliss$conc, weights = w)
	cat(i, coef(lmod), "\n")
}

summary(lmod)	# SE not correct; to compute:
xm = model.matrix(lmod)
wm = diag(w)
sqrt(diag(solve(t(xm) %*% wm %*% xm)))	# for binomial 

# To get the correct SE in the summary of lm
summary(lmod)$coef[, 2] # SE as is, correct by:
summary(lmod)$coef[, 2] / summary(lmod)$sigma 
# ... but all this can be done more efficiently via the glm function



# 6.3 Hypothesis Tests
summary(modl)

# test goodness of fit by comparing res.dev to res. df:
1 - pchisq(deviance(modl), df.residual(modl))	
# 0.9446 (large p: no evidence of lack of fit)

# or, comparing to the null:
anova(modl, test = "Chi")	# clearly better than null

# Test a more complex model
modl2 = glm(cbind(dead, alive) ~ conc + I(conc^2), family = binomial, bliss)
summary(modl2)
anova(modl2, test = "Chi")	# quadratic term not signif; reduces to previous



# 6.4 GLM Diagnostics
resid(modl)	# Deviance residuals
resid(modl, "pearson")
resid(modl, "response")
resid(modl, 'working')

resid(lmod)


# Leverage and Influence
influence(modl)$hat	# leverages -- all roughly equal - good
rstudent(modl)	# studentized resids
# change in fit due to omitting a case can be done by comparing coefs:
influence(modl)$coef	#in this case datum 1 has largest influence
cooks.distance(modl)


# Model Diagnostics
# Give the typically non-normal distribution of residuals, it is generally 
# more informative to plot the linear predictors (eta.hat) than the predicted
# response (mu.hat) as with lms.
data(gala)
gala = gala[,-2]; head(gala)
modp = glm(Species ~., family = poisson, gala)
plot(resid(modp) ~ predict(modp, type = "response"), 
	 xlab = expression(hat(mu)), ylab = "Deviance residuals")
plot(resid(modp) ~ predict(modp, type = "link"), xlab = expression(hat(eta)),
 	 ylab = "Deviance residuals")
# Note, this (et.hat) is the first plot given for plot(model.glm):
par(mfrow = c(2, 2))
plot(modp)

# By plotting the response, we can unscale the variance to view the (known) 
# non-constant nature
plot(resid(modp, type = "response") ~ predict(modp, type = "link"), 
	 xlab = expression(hat(eta)), ylab = "Response residuals")
# The response variable is count (Poisson-distributed), so this is expected, 
# and why we want to use a glm in the first place


plot(Species ~ Area, gala)
plot(Species ~ log(Area), gala)
mu = predict(modp, type = "response")
# linearize the response to meet glm assumptions
# z = eta         + (y            - mu) / mu == eta + (y - mu) * (deta/dmu)
z = predict(modp) + (gala$Species - mu) / mu
plot(z ~ log(Area), gala, ylab = "Linearized response")

# In this model, log transformations of all variables help to linearize
modpl = glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + 
			log(Scruz + 0.1) + log(Adjacent), family = poisson, gala)
c(deviance(modp), deviance(modpl)) # substantial reduction in deviance

# Linearizing variables with respect to the response only may be problematic
# though, as it fails to account for the effects of the other variable.
# For lms, partial resid plots are used for this purpose; in glms they can be
# applied as: z - eta.hat + beta[j]x[j] ~ x[j]
# For example, the log(Area) predictor:
mu = predict(modpl, type = "response")
#u = z - eta.hat             + beta[Area]    *x
u = (gala$Species - mu) / mu + coef(modpl)[2]*log(gala$Area)
plot(u ~ log(Area), gala, ylab = "Partial Residual")
abline(0, coef(modpl)[2], col = 2) # No apparent reason for concern

# The same can be achieved with:
residuals(modpl, type = "partial")	# Partial residuals by predictor
plot(residuals(modpl, type = "partial")[,1] ~ log(gala$Area)) 

# Check assumption of validitiy of the particular link function: (plot z 
# against eta)
z = predict(modpl) + (gala$Species - mu) / mu
plot(z ~ predict(modpl), xlab = "Linear predictor", 
	 ylab = "Linearized response")

# Unusual Points
# Look for outliers in the fitted residuals:
# Because in glms we do not expect resids to be normal, we don't expect a 
# straight line in the halfnorm plot, but want to identify points that 
# deviate from the trend
halfnorm(rstudent(modpl)) # of jackknife residuals; no apparent outliers

gali = influence(modpl)
halfnorm(gali$hat)	# distriubution of leverages by predictor 
					# (e.g., 25 = Scruz may have undue leverage)
halfnorm(cooks.distance(modpl))	# distribution of Cook's D by predictor. 
								# Again Scruz is 'outlier'

plot(gali$coef[, 5], ylab = "Change in Scruz Coef", xlab = "Case no.")	
# [, 5] == Scruz; Case 25 (Santa Cruz island itself; [Scruz = 0]) has biggest 
# effect, cf. model w/o 25:

# refit model without this datum
modplr = update(modpl, ~., subset = -25)
cbind(coef(modpl), coef(modplr))

# Final model:
modpla = glm(Species ~ log(Area) + log(Adjacent), family = poisson, gala)
dp = sum(resid(modpla, type = "pearson")^2) / modpla$df.res
summary(modpla, dispersion = dp)	
# same as modeling with quasipoisson dispersion parameter
modpla2 = glm(Species ~ log(Area) + log(Adjacent), family = quasipoisson, 
			  gala)
summary(modpla2)






save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')