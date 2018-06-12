#=======================================================================#
#                                                                       #
#  Extending the Linear Model with R:                                   #
#    Generalized Linear, Mixed Effects and NonParametric Regression     #
#    Models   Faraway (2006)                                            #
#                                                                       #
#                   #===================================================#
#                   #
#  7. Other GLMs    #
#                   #
#===================#
rm(list = ls())
setwd('~/Learning/R/RLearning/ExtendingLM/')

library(faraway)
library(MASS)
library(statmod)
data(cpd)
data(motorins)
data(wafer)
data(weldstrength)

# 1 Gamma GLM
# Used for continuous, skewed responses
# Some examples of the gamma distribution:
x <- seq(0, 8, 0.001)
r <- 1
b <- 0
color <- rgb(r, 0, b)
plot(x, 
     dgamma(x, 0.75), 
     type='l', 
     xlab='', 
     ylab='', 
     ylim=c(0, 1.25), 
	 xaxs='i',          #'i's make 0 flush with axes
	 yaxs='i', 
	 col=color)	
for(s in seq(0.8, 5.0, 0.05)) {
  r <- r - 0.01
  b <- b + 0.01
  lines(x, dgamma(x, s), col=rgb(r, 0, b))
}

head(wafer)
summary(wafer)
hist(wafer$resist, freq=F)
lines(density(wafer$resist))
rug(wafer$resist)

llmdl <- lm(log(resist) ~ .^2, wafer)
rlmdl <- step(llmdl)
summary(rlmdl)

gmdl <- glm(resist ~ .^2, family=Gamma(link=log), wafer)
rgmdl <- step(gmdl)
summary(rgmdl)

gamma.dispersion(rgmdl)

head(motorins)
motori <- motorins[motorins$Zone == 1, ]
hist(motori$Payment)
gl <- glm(
  Payment ~ offset(log(Insured)) + as.numeric(Kilometres) + Make + Bonus, 
  family=Gamma(link=log), 
  motori)
summary(gl)

# cf lognormal model:
llg <- glm(
  log(Payment) ~ offset(log(Insured)) + as.numeric(Kilometres) + Make + Bonus, 
  family=gaussian, 
  motori)
summary(llg)

x <- seq(0,5, 0.05)
plot(x, 
     dgamma(x, 1 / 0.55597, scale=0.55597), 
     type='l', 
     ylab='', 
     xlab='', 
	 xaxs='i', 
	 yaxs='i', 
	 ylim=c(0, 1))
lines(x, dlnorm(x, meanlog=-0.30551, sdlog=sqrt(0.61102)), col=2)
x0 <- data.frame(Make='1', Kilometres=1, Bonus=1, Insured=100)
predict(gl, new=x0, se=T, type='response')
# and for lognormal:
predict(llg, new=x0, se=T, type='response')	
# and to rescale to original scale:
c(exp(10.998), exp(10.998)*0.16145)



# 2 Inverse Gaussian GLM
x <- seq(0, 8, 0.01)
r <- 1
b <- 0
color <-rgb(r, 0, b) 
plot(x, 
     dinvgauss(x, 1, 0.5),
     xlab='', 
     ylab='', 
     ylim=c(0, 1.5), 
     xaxs='i', 
	 yaxs='i', 
	 type='l', 
	 col=color)
	 
for (i in seq(1, 3, length=10)) {
  r <- r - 0.09
  for (j in seq(1, 5, length=10)) {
    b <- b + 0.09
    if (b > 1) { b=0 }
      lines(x, dinvgauss(x, i, j), col=rgb(r, 0, b))
  }
}

head(cpd)
plot(cpd$actual ~ cpd$projected)
lmod <- lm(actual ~ projected - 1, cpd)
summary(lmod)
igmod <- glm(actual ~ projected - 1, 
			 family=inverse.gaussian(link='identity'), 
			 cpd)
summary(igmod)
plot(resid(igmod) ~ log(fitted(igmod)), 
     ylab='Dev. Resid', 
	 xlab=expression(log(hat(mu))))
abline(h=0)



# 3 Joint Modeling of the Mean and Dispersion
lmod <- lm(Strength ~ Drying + Material + Preheating, weldstrength)
summary(lmod)

h <- influence(lmod)$hat
d <- resid(lmod)^2 / (1 - h)
gmod <- glm(d ~ Material + Preheating, 
            family=Gamma(link=log), 
		    weldstrength, 
		    weights=1 - h)
w <- 1 / fitted(gmod)
lmod <- lm(Strength ~ Drying + Material + Preheating, weldstrength, weights=w)
summary(lmod)
summary(gmod)


# 7.4 Quasi-Likelihood
data(mammalsleep)
mammalsleep$pdr = with(mammalsleep, dream / sleep) # % of sleep that is dream
summary(mammalsleep$pdr)
hist(mammalsleep$pdr)
pairs(mammalsleep, panel = panel.smooth)

modl = glm(pdr ~ log(body) + log(brain) + log(lifespan) + log(gestation) +
		   predation + exposure + danger, family = quasibinomial, 
		   mammalsleep)
summary(modl)
drop1(modl, test = 'F') # Suggest elimination of predation, least signif
# ...
modl = glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, 
		   mammalsleep)
summary(modl)
# NOTE: dispersion (0.041) much less than for true binomial; also as there is no true likelihood function, AIC is NA
# ALSO: resid deviance is still large relative to null dev--not a good fit

# Some diagnostics
ll = row.names(na.omit(mammalsleep[, c(1, 6, 10, 11)]))
halfnorm(cooks.distance(modl), labs = ll) # Asian elephant has high influence
plot(predict(modl), resid(modl, type = 'pearson'), 
	 xlab = 'Linear Predictor', ylab = 'Pearson Residuals')
# Variance is basically constant, so variance function ([quasi-]binom) seems appropriate





save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')