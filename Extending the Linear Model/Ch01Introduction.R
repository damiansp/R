#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#=======================#
#						#
#	1.	Introduction	#
#						#
#=======================#
rm(list=ls())
library(faraway)
library(MASS)
library(splines)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')
#data(gavote)



head(gavote)
summary(gavote)

gavote$undercount <- (gavote$ballots - gavote$votes) / gavote$ballots
truehist( gavote$undercount, main='Undercount', xlab='% undercount', 
		  ylim=c(0, 20) )
lines(density(gavote$undercount))
rug(gavote$undercount)

pie(table(gavote$equip))
barplot(sort(table(gavote$equip), T))

gavote$perGore <- gavote$gore / gavote$votes
plot(perGore ~ perAA, data=gavote, xlab='Proportion Black Voters')

plot(undercount ~ equip, data=gavote, xlab='', las=3)

xtabs(~ atlanta + rural, data=gavote)
table(gavote$atlanta, gavote$rural)

corDat <- c(3, 10, 11, 12)
cor(gavote[, corDat])

lmod <- lm(undercount ~ perGore + perAA, data=gavote)
summary(lmod)

par(mfrow=c(1,2))
plot(undercount ~ perGore, data=gavote)
plot(undercount ~ perAA, data=gavote)
range(predict(lmod))
deviance(lmod)	# for lm = resid. sum of squares
df.residual(lmod) # same as:
nrow(gavote) - length(coef(lmod))
sqrt(deviance(lmod) / df.residual(lmod))	# resid SE = sd of the error:
summary(lmod)$sigma
summary(lmod)$r.squared	# same as:
cor(predict(lmod), gavote$undercount)^2
summary(lmod)$adj.r.squared

gavote$cPerGore <- gavote$perGore - mean(gavote$perGore)
gavote$cPerAA <- gavote$perAA - mean(gavote$perAA)

lmodi <- lm(undercount ~ cPerAA + cPerGore * rural + equip, data=gavote)
summary(lmodi)

anova(lmod, lmodi)	# the p value indicates a sig diff in the performance of 					# the models, suggesting a preference for the larger mod
drop1(lmodi, test='F') 
summary(lmodi)
confint(lmodi)

par(mfrow=c(2, 2))
plot(lmodi)
gavote[cooks.distance(lmodi) > 0.1, ]

halfnorm(influence(lmodi)$hat)	# leverages
gavote[influence(lmodi)$hat > 0.3, ]	# The only two counties with paper 											# ballots, hence the only est for that
										# coef.
termplot(lmodi, partial=T, terms=1)	# These show err + beta[i]*x[i] against 
termplot(lmodi, partial=T, terms=2)	# x[i], e.g., the error just relative to 
termplot(lmodi, partial=T, terms=3)	# a single predictor
# The termplots are good for eyeballing the relationships between individual predictors and the response to check for non-linearity, etc. possibly necessitating a transformation

# rlm ('robust' lm) is a linear regression that downweights points with larger errors
rlmodi <- rlm(undercount ~ cPerAA + cPerGore * rural + equip, gavote)
summary(rlmodi)
# weighted least squares gives more weight based on another numerical term, here, the no. of ballots, since estimates from counties with more ballots should yield more precise estimates
wlmodi <- lm( undercount ~ cPerAA + cPerGore * rural + equip, gavote, 
			  weights=ballots )
summary(wlmodi)	# but see comments on p 18 for why this is not a good idea here

# othogonal polynomial transformation of predictors:
plmodi <- lm(undercount ~ poly(cPerAA, 4) + cPerGore * rural + equip, gavote)
summary(plmodi)
termplot(plmodi, partial=T, terms=1)

# bs() uses B-splines
blmodi <- lm(undercount ~ cPerAA + bs(cPerGore, 4) + rural + equip, gavote)
summary(blmodi)
termplot(blmodi, partial=T, terms=2)

biglm <- lm( undercount ~ (equip + econ + rural + atlanta)^2 + 
			 (equip + econ + rural + atlanta) * (perAA + perGore), gavote )
smallm <- step(biglm, direction = 'both', trace = F)
summary(smallm)

drop1(smallm, test='F')

finalm <- lm( undercount ~ equip + econ + perAA + equip:econ + equip:perAA,
			  gavote )

# Predict undercounts for multiple combinations of predictors
pdf <- data.frame( econ=rep(levels(gavote$econ), 5), 
				   equip=rep(levels(gavote$equip), rep(3, 5)), perAA=0.233 )
pp <- predict(finalm, new=pdf)
xtabs(round(pp, 3) ~ econ + equip, pdf)

pdf <- data.frame( econ=rep('middle', 15), 
				   equip=rep(levels(gavote$equip), rep(3, 5)), 
				   perAA=rep(c(0.11, 0.23, 0.35), 5) )
pp <- predict(finalm, new=pdf)
propAA <- gl(3, 1, 15, labels=c('low', 'medium', 'high'))
xtabs(round(pp, 3) ~ propAA + equip, pdf)



	

save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')