#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===========================#
#							#
#	3.	Count Regression		#
#							#
#===========================#
rm(list=ls())
library(faraway)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')

#data(gala)


# 3.1 Poisson Regression
# If counts are numbers of a larger population (proportions) binomial models should be used; if n is large and ps are small, poisson is a good approximation
gala <- gala[, -2]

modl <- lm(Species ~ ., gala)
modt <- lm(sqrt(Species) ~ ., gala)

par(mfrow=c(2, 1))
plot(predict(modl), resid(modl), xlab='', ylab='Residual', main='Species')
plot( predict(modt), resid(modt), xlab='Fitted', ylab='Residual', 
	  main='srqrt(Species)' )
# sqrt() model clears up heteroskedacity

summary(modt)

# Not bad, but a Poisson might be more appropriate:
# For Poisson, the response is distributed according to the parameter mu (= mean = sd), which must be non-negative, hence the log link function is appropriate: log(mu[i]) = eta[i] = t(x[i])*beta; The model now has a linear predictor and log-likelihood is: 
# l(beta) = sum(y[i] * t(x[i])*beta - exp(t(x[i])*beta) - log(factorial(y[i]))
# Differentiate w respect to beta[j] for MLE solution:
# sum(( y[i] - exp(t(x[i])) ) * x[i, j]) = 0 (All j); or:
# t(X) * y = t(X) * mu.hat
modp <- glm(Species ~ ., family=poisson, gala)
summary(modp)
# resid deviance of 717 rel to 24 df suggests a poor fit--check resids for possible outliers
par(mfrow=c(1, 1))
halfnorm(resid(modp)) 	# no apparent outlier; also proportion of the deviance 						# explained by the model is appx = to that of the lm()
1 - (716.85 / 3510.73)	# 79.6% (vs. 78.3 for lm(sqrt() ~ .))

# For Poisson, mean and var are equal... investigate relationship in model:
# Var for each given point est as (y - mu.hat)^2
plot( log(fitted(modp)), log((gala$Species - fitted(modp))^2),
	  xlab=expression(hat(mu)), ylab=expression((y - hat(mu))^2))
abline(0, 1, col='grey')
abline(lm(log((gala$Species - fitted(modp))^2) ~ log(fitted(modp))), col=2)
legend( 'bottomright', lty=1, col=c('grey', 'red'), 
		legend=c('Perfect Poisson', 'Model') )
# Model var is proportional to, but larger than, the mean... in such cases, estimates of coefs will be consistent, but SEs will be wrong, hence signif. cannot be determined
# A dispersion parameter can be estimated (see p. 59-60 for details)
(dp <- sum(resid(modp, type='pearson')^2) / modp$df.resid)	# 31.749 (!)
# Use to correct SEs:
summary(modp, dispersion=dp)
# When comparing overdispersed (or under-) Poisson models, an F-test should be used instead of Chisq
drop1(modp, test='F')	# generally preferrable to z-stats in summary() 



# 3.2 Rate Models
data(dicentric)
round(xtabs(ca / cells ~ doseamt + doserate, dicentric), 2) # ca = chromosomal 															# abnormalities
with(dicentric, interaction.plot(doseamt, doserate, ca / cells, col=1:9))
# doserate could be multiplicative, model as log:
lmod <- lm(ca / cells ~ log(doserate) * factor(doseamt), dicentric)
summary(lmod)
# good fit, but:
par(mfrow=c(2, 2))
plot(lmod)	# ...serious heteroskedacity introduced

# Model count response directly:
dicentric$dosef <- factor(dicentric$doseamt)
# log(cells) b/c also expected to have mult effect on response
pmod <- glm(ca ~ log(cells) + log(doserate) * dosef, family=poisson, dicentric)
summary(pmod)
plot(pmod)
 
# Note from the summary, the coef for log(cells) appx 1, fix as 1 for the rate model:
rmod <- glm( ca ~ offset(log(cells)) + log(doserate) * dosef, family=poisson, 
			 dicentric )
summary(rmod)	# resid dev. indicates a good fit...
plot(rmod)		# ...and diagnostics are pretty good



# 3.3 Negative Binomial
# Ways the negative binomial may arise naturally: 
# Given a system that can withstand k hits, with a prob of a hit = p within a given time period; generalization of Poisson when lambda is gamma distributed
data(solder)
modp <- glm(skips ~ ., family=poisson, data=solder)
summary(modp)	# note resid dev / df = poor fit

# Try adding interactions:
modp2 <- glm( skips ~ (Opening + Solder + Mask + PadType + Panel)^2, 
			  family=poisson, data=solder )
summary(modp2)	# Fit is still poor
plot(modp2)		# ...even with pretty good diagnostics

# Try modeling as negative binomial
modn <- glm(skips ~ ., family=negative.binomial(1), data=solder)
summary(modn)
plot(modn)	# much better fit and ok

# Can further improve by estimating the k parameter of the negative binomial (e.g.: P(Z = z) = choose(z - 1, k - 1) * p^k * (1 - p)^(z - k); z = k, k + 1, ...)
modn2 <- glm.nb(skips ~ ., data=solder)
summary(modn2)	# Theta (4.397 ± 0.495) is the estimate of k
plot(modn2)







save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')