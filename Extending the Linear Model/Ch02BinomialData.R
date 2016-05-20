#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#=======================#
#						#
#	2.	Binomial Data	#
#						#
#=======================#
rm(list=ls())
library(brglm)
library(faraway)
library(MASS)
library(survival)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')

#data(amlxray)
#data(babyfood)
#data(bliss)
#data(hormone)
#data(orings)
#data(troutegg)


# 2.1 Challenger Disaster Example
plot(damage / 6 ~ temp, orings, xlim=c(25, 85), ylim=c(0, 1))

# Wrong approach:
lmod <- lm(damage / 6 ~ temp, orings)
abline(lmod)



# 2.2 Binomial Regression Model
# Linear predictor: η[i] = β[0] + β[1]x[i, 1] + ... + β[q]x[i, q]
# Link function, such that η[i] = g(p[i]), with 0 ≤ g^-1(η) ≤ 1, such as:
#	logit: η = log(p / (1 - p)) 
#	probit: η = Φ^-1(p)		[Φ is normal cdf]
#	Complementary log-log: η = log(-log(1 - p))
# In R: glm(cbind(successes, failures) ~ pred, family=binomial, data); logit is the default link function (AKA: logistic regression)
logitmod <- glm(cbind(damage, 6 - damage) ~ temp, family=binomial, orings)
summary(logitmod)

x <- seq(25, 85, 1)
lines(x, ilogit(11.66299 - 0.21623*x), col=2)

probitmod <- glm( cbind(damage, 6 - damage) ~ temp, 
				  family=binomial(link=probit), orings )  
summary(probitmod)
lines(x, pnorm(5.59145 - 0.10580*x), col=3)

# predicted probabilities of failure at Temp = 31°F
ilogit(11.66299 - 0.21623*31)	# 0.9930
pnorm(5.59145 - 0.10580*31)		# 0.9896



# 2.3 Inference
# Use deviance test (p. 29) to determine if model is an adequate fit
pchisq(deviance(logitmod), df.residual(logitmod), lower=F)	# 0.7164
# The high p value indicates a relatively good fit

summary(logitmod)
confint(logitmod)



# 2.4 Tolerance Distribution
# 2.5 Interpreting Odds
xtabs(disease / (disease + nondisease) ~ sex + food, babyfood)

mdl <- glm(cbind(disease, nondisease) ~ sex + food, family=binomial, babyfood)
summary(mdl)

# since resid dev (0.72) is small relative to the resid df (2), an interaction effect is unlikely, and we may interpret main effects individually:
drop1(mdl, test="Chi") # Tests each predictor relative to the full model

# Now consider the interpretation of the coefs; for breast-feeding:
exp(coef(mdl)[3])	# 0.51: breast-feeding reduces the odds of respiratory 
					# disease to 51% of that of bottle feeding
exp(confint(mdl))	# or, the 95%CI for breastfeeding odds: [0.38, 0.69]



# 2.6 Prospective and Retrospective Sampling
babyfood[c(1, 3),]	# just boys, and ignoring Supplemented feeding
# log(odds of disease | breast-fed):
log(47 / 447)	# -2.25
# log(odds of disease | bottle-fed):
log(77 / 381)	# -1.60
# The difference of the two log-odds:
-1.60 - -2.25	# 0.65 represents the increased risk of disease for bottle fed
				# boys or the "log-odds ratio"



# 2.7 Choice of Link Function
bliss	# data of insects surviving difft concentrations of insecticide
# Fit all 3 link functions to the data
modl <- glm(cbind(dead, alive) ~ conc, family=binomial, data=bliss) # logit
modp <- glm( cbind(dead, alive) ~ conc, family=binomial(link='probit'), 
			 data=bliss ) 
modc <- glm( cbind(dead, alive) ~ conc, family=binomial(link='cloglog'), 
			 data=bliss ) 

fitted(modl)		# same as:
predict(modl, type='response')
predict(modl)	# predictions in model form (i.e., in logits); same as:
modl$linear.predictors
ilogit(modl$lin)		# same as above

# compare fits with each link function
modfits <- cbind(fitted(modl), fitted(modp), fitted(modc))
colnames(modfits) <- c('logit', 'probit', 'comp.log-log')
modfits

# look over a wide range of vals
x <- seq(0, 8, 0.1)
pl <- predict(modl, newdata=data.frame(conc=x), type='response')
pp <- predict(modp, newdata=data.frame(conc=x), type='response')
pc <- predict(modc, newdata=data.frame(conc=x), type='response')

plot(x, pl, type='l', ylab='Probability', xlab='Dose')
lines(x, pp, col=2)
lines(x, pc, col=4)
legend( 'bottomright', lty=1, col=c(1, 2, 4), 
		legend=c('logit', 'probit', 'complementary log-log'), 
		title='Link Function' )

# Compare relative differences
matplot( x, cbind(pp / pl, (1 - pp) / (1 - pl)), type='l', xlab='Dose', 
		 ylab='Ratio (p / l)' )
matplot( x, cbind(pc / pl, (1 - pc) / (1 - pl)), type='l', xlab='Dose', 
		 ylab='Ratio (c / l)' )



# 2.8 Estimation Problems
hormone
plot(estrogen ~ androgen, data=hormone, col=orientation)
legend('topleft', pch=1, col=1:2, legend=levels(hormone$orientation), 
		title='Orientation' )
# The following DOES NOT converge:
modl <- glm(orientation ~ androgen + estrogen, hormone, family=binomial)
summary(modl)	# obviously problematic: resid dev is extremely small, even 
				# though none of the predictors is signif.
				# This is b/c data are linearly separable allowing a perfect
				# fit
abline(-84.49 / 90.22, 100.91 / 90.22, col='grey')
# In such cases exact logistic regression may be preferred (no such R package), or the bias-reduction method (see p. 39 for refs)  
modb <- brglm(orientation ~ androgen + estrogen, hormone, family=binomial)
summary(modb)

# similar instability in param estimation will occur even at near-linear-separability conditions



# 2.9 Goodness of Fit
modl <- glm(cbind(dead, alive) ~ conc, family=binomial, data=bliss)
sum(residuals(modl, type='pearson')^2)	# This is a proposed g-o-f stat
deviance(modl)	# ...and is genersally similar to this.

# r.sq for binomial model (150 = n)
(1 - exp((modl$dev - modl$null) / 150)) / (1 - exp(-modl$null / 150))



# 2.10 Prevention and Effective Dose
# Making predictions for a given set of covariate values
(lmodsum <- summary(modl))

# Predict response for dose of 2.5:
x0 <- c(1, 2.5)  # 1 for the inercept
eta0 <- sum(x0 * coef(modl))
ilogit(eta0)		# i.e., 64% chance of death at this dose

# And for the 95% CI:
(cm <- lmodsum$cov.unscaled)
# SE on logit scale:
se <- sqrt(t(x0) %*% cm %*% x0)
ilogit(c(eta0 - 1.96*se, eta0 + 1.96*se))	# 95% CI: [0.534, 0.736]

# Or, more simply:
predict(modl, newdata=data.frame(conc=2.5), type='response', se=T)
pred <- predict(modl, newdata=data.frame(conc=2.5), se=T)
fit <- pred$fit
se <- pred$se.fit
ilogit(c(fit - 1.96*se, fit + 1.96*se))

# To find the 50% effective (or lethal) dose (ED50, LD50):
(ld50 <- -modl$coef[1] / modl$coef[2])
# ...and the SE on that term:
# var g(theta.hat) appx = g'(theta.hat)^T * var(theta.hat) * g' (theta.hat)
# or:
(dr <- c(-1 / modl$coef[2], modl$coef[1] / modl$coef[2]^2))
(se <- sqrt(dr %*% lmodsum$cov.un %*% dr)[,])
(ci95 <- c(2 - 1.96*se, 2 + 1.96*se))

# Similarly, the ED (LD) x[p] for prob. of success p is:
# x[p] = (logit(p) - beta0) / beta1:
# So for 90% prob of being a lethal dose of insecticide:
(ed90 <- (logit(0.9) - modl$coef[1]) / modl$coef[2])
# The same can be determined from MASS::dose.p():
dose.p(modl, p=c(0.5, 0.9))



# 2.11 Overdispersion
par(mfrow=c(2, 2))
plot(modl)
# For a correct model specification, error (resid. dev.) should be distributed as chi.sq w appropriate df.
ftable(xtabs(cbind(survive, total) ~ location + period, data=troutegg))
# Note extreme cases (location, period) = (4, 4): all eggs suvive, and (5, 11): no eggs survive
bmod <- glm( cbind(survive, total - survive) ~ location + period, 
			 family=binomial, data=troutegg )
summary(bmod)
# The high resid. dev. relative to the 12 resid. df indicates a poor fit
plot(bmod)
troutegg[c(14, 15, 19, 20), ]	# apparent outliers, one of which is the no-
								# survivor case

par(mfrow=c(1, 1))
halfnorm(resid(bmod))	# 'nother way to check for outliers, here they 
						# actually look pretty good
# Can also plot empirical logits: 
# log((y + 0.5) / (m - y + 0.5))
elogits <- log( (troutegg$survive + 0.5) / 
				(troutegg$total - troutegg$survive + 0.5) )
with(troutegg, interaction.plot(period, location, elogits, col=1:5)) 
# No obvious sign of interactions (e.g., all locations show similar curves)

# Having eliminated outliers and poor model designation, overdispersion seems likely to be the problem
# Estimate dispersion:
(sigma2 <- sum(resid(bmod, type='pearson')^2) / 12)	# 12 = resid df.
# Very high dispersion paramater (= 1 if not over/under-dispersed)

drop1(bmod, scale=sigma2, test='F')
# Both predictors clearly signif.
# With overdispersion we cannot compute goodness of fit, but can correct the SEs for the predictors:
summary(bmod)
summary(bmod, dispersion=sigma2)



# 2.12 Matched Case-Control Studies
head(amlxray)
amlxray[amlxray$downs == 'yes', 1:4]
# all are positive cases (disease = 1), so est. coef will be Inf; remove
(ii <- which(amlxray$downs == 'yes')) 
ramlxray <- amlxray[-c(ii, ii + 1),]	# remove match as well

# Conditional Logit Model:
cmod <- clogit(disease ~ Sex + Mray + Fray + CnRay + strata(ID), ramlxray)
summary(cmod)

# since there is only a sig. linear effect for (ordered) CnRay, convert to numerical:
cmodr <- clogit(disease ~ Fray + unclass(CnRay) + strata(ID), ramlxray)
summary(cmodr)

# NOTE: for matched data the glm(... family=binomial) approach is not suitable










save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')