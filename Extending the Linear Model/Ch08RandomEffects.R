#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#=======================#
#						#
#	8. Random Effects	#
#						#
#=======================#
rm(list = ls())
library(faraway)
library(lme4)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')

# 8.1 Estimation
data(pulp)
head(pulp)

op = options(contrasts = c('contr.sum', 'contr.poly'))
lmod = aov(bright ~ operator, pulp)
summary(lmod)
coef(lmod)
options(op)

# For sum contrasts, the sum of the coefficients is 0 so, the effect of the fourth operator is:
-sum(coef(lmod)[2:4])
# The variance of operator effects is computed as:
# (MSE(operator) - MSE(resid)) / params 
(0.4467 - 0.1062) / 5

# ML estimators
mmod = lmer(bright ~ 1 + (1 | operator), pulp)
summary(mmod)
# Note resid var est (0.1063) is the same as MSE(resid) above, as is the overall mean (intercept), 60.40

# ML estimates can also be computed as:
#smod = lmer(bright ~ 1 + (1 | operator), pulp, REML = 'ML') # DEPRECATED



# 8.2 Inference
nullmod = lm(bright ~ 1, pulp)
summary(nullmod)
anova(smod, nullmod)

y = simulate(nullmod)

# bootstrap to improve inference of p value in anova above
lrstat = numeric(1000)
for (i in 1:1000) {
	y = unlist(simulate(nullmod))
	bnull = lm(y ~ 1)
	balt = lmer(y ~ 1 + (1 | operator), pulp, method = 'ML')
	lrstat[i] = as.numeric(2*logLik(balt) - logLik(bnull))
}

# compute proportion close to 0
sum(lrstat < 0.00001) / 1000 # Problem because of deprecated smod usage



# 8.3 Predicting Random Effects
ranef(mmod)
(cc = model.tables(lmod))
cc[[1]]$operator / ranef(mmod)$operator
# i.e., predicted random effects are precisely proportionate to the fixed effects
# Thus not knowing the operator our expected y is simply the overall mean:
mean(pulp$bright)
# but knowing the operator we can calculate the best linear unbiased predictor (blup) as:
fixef(mmod) + ranef(mmod)$operator
# Diagnostics:
qqnorm(resid(mmod))
qqline(resid(mmod))
plot(fitted(mmod), resid(mmod)); abline(h = 0)



# 8.4 Blocks as Random Effects
data(penicillin)
head(penicillin)

op = options(contrasts = c('contr.sum', 'contr.poly'))
lmod = aov(yield ~ blend + treat, penicillin)
summary(lmod)

# Now model with fixed treatment, but random blend, effects
mmod = lmer(yield ~ treat + (1 | blend), penicillin)
summary(mmod)

options(op)

# Blups for random effects:
ranef(mmod)$blend

# Test signif of fixed effects - option 1:
anova(mmod)

# option 2: ML ratio method
amod = lmer(yield ~ treat + (1 | blend), penicillin, REML = F)
nmod = lmer(yield ~ (1 | blend), penicillin, REML = F)
anova(amod, nmod)

# Bootstrap to improve accuracy
lrstat = numeric(1000)
for (i in 1:1000) {
	ryield = unlist(simulate(nmod))
	nmodr = lmer(ryield ~ 1 + (1 | blend), penicillin, REML = F)
	amodr = lmer(ryield ~ treat + (1 | blend), penicillin, REML = F)
	lrstat[i] = 2 * (logLik(amodr) - logLik(nmodr))
}

# Stat should have a chi-sq[3] distribution:
plot(qchisq((1:1000) / 1001, 3), sort(lrstat), xlab = expression(chi[3]^2), 
	 ylab = 'Simulated LRT')
abline(0, 1)
# Approximation is poor.  Can compute p value as the mean of simulated values > the ChiSq stat for the last ANOVA:
mean(lrstat > 4.0474)

# Fit mod w/o random effects and compute LRT
rmod = lmer(yield ~ treat + (1 | blend), penicillin)
nlmod = lm(yield ~ treat, penicillin)
2 * (logLik(rmod) - logLik(nlmod, REML = T))

# ...and bootstrap:
lrstatf = numeric(1000)
for (i in 1:1000) {
	ryield = unlist(simulate(nlmod))
	nlmodr = lm(ryield ~ treat, penicillin)
	rmodr = lmer(ryield ~ treat + (1 | blend), penicillin)
	lrstatf[i] = 2 * (logLik(rmodr) - logLik(nlmodr, REML = T))
}

mean(lrstatf < 0.00001) # clearl not chi-sq distributed
cs = lrstatf[lrstatf > 0.00001]
ncs = length(cs)

plot(qchisq((1:ncs) / (ncs + 1), 1), sort(cs), xlab = expression(chi[1]^2), 
	 ylab = 'Simulated LRT')
abline(0, 1)

# p val
mean(lrstatf > 2.762908)
# marginally significant blend effect.
# Take home: basically get the same results as with simpler fixed effects models for blocks.  But....


# 8.5 Split plots
data(irrigation)
head(irrigation)
lmod = lmer(yield ~ irrigation * variety + (1 | field) + 
			(1 | field:variety), data = irrigation)



# 8.6 Nested Effects











save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')