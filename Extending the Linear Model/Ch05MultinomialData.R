#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===========================#
#							#
#	5.	Multinomial Data	#
#							#
#===========================#
rm(list = ls())
library(faraway)
library(MASS)
library(nnet)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')

# 5.1 Multinomial Logit Model 
# Used for nominal (not ordered) data
data(nes96)	# Election data: treat party ID as response to be predicted
sPID = nes96$PID
# Relevel to collapse all types of democrat, independent, or republican
levels(sPID) = c( "Democrat", "Democrat", "Independent", "Independent", 
				  "Independent", "Republican", "Republican" )
summary(sPID)
# Convert income to numeric vals
inca = c( 1.5, 4, 6, 8, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 16, 18.5, 21, 23.5, 
		  27.5, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 82.5, 97.5, 115 )
nincome = inca[unclass(nes96$income)]
truehist(nincome)
lines(density(nincome))
table(nes96$educ)

matplot( prop.table(table(nes96$educ, sPID), 1), type = "l", xlab = "education", 
		 ylab = "proportion", col = c(1, 2, 4))
# Cut income into 7 (ordered) categories
cutinc = cut(nincome, 7) 
# Label each with its middle value
il = c(8, 26, 42, 58, 74, 90, 107) 
matplot( il, prop.table(table(cutinc, sPID),1), type = "l", ylab = "proportion", 
		 xlab = "income", col = c(1, 2, 4) )
cutage = cut(nes96$age, 7)
al = c( 24, 34, 44, 54, 65, 75, 85)
matplot( al, prop.table(table(cutage, sPID),1), type = "l", ylab = "proportion", 
		 xlab = "age", col = c(1, 2, 4) )

# Are trends in proportions significant?
#library(nnet)
mmod = multinom(sPID ~ age + educ + nincome, nes96)
summary(mmod)
mmodi = step(mmod, direction = 'both')
summary(mmodi)

mmode = multinom(sPID ~ age + nincome, nes96)
deviance(mmode) - deviance(mmod)
pchisq(16.21, mmod$edf - mmode$edf, lower = F)	# education not significant 
												# relative to full model

predict(mmodi, data.frame(nincome = il), type = "probs")	# row values are 	
															# increasing incomes
predict(mmodi, data.frame(nincome = il)) 	# i.e. expected value for each 
											# income level
summary(mmodi)	
# Intercept models prob of party identification w/ income  = 0; Slope term (nincome) represents log-odds of moving from baseline category (Democrat here) to Indpendent or Republican, per unit change (of $1000 here) in income 
cc = c(0, -1.1749, -0.9504)		# intercepts
exp(cc) / sum(exp(cc))	# transforms back to exp proportions at income = 0, c.f.

predict(mmodi, data.frame(nincome = 0), type = "probs")
# for slope (nincome):
(pp = predict(mmodi, data.frame(nincome = c(0, 1)), type = "probs"))
log(pp[1, 1] * pp[2, 2] / (pp[1, 2] * pp[2, 1]))		# compares Dem to Ind
log(pp[1,1]*pp[2,3] / (pp[1,3]*pp[2,1]))

# Model as poisson-family GLM (resp as 1 for one factor, and 0 for all others)
sPID[1:4]
cm = diag(3)[unclass(sPID),]
cm[1:4,] # representation of sPID[1:4] above
y = as.numeric(t(cm))
resp.factor = gl(944, 3)
cat.factor = gl(3, 1, 3 * 944, labels = c("D", "I", "R"))
rnincome = rep(nincome, each = 3)
head(data.frame(y, resp.factor, cat.factor, rnincome)) 
# first 3 rows all represent first individual, 2nd 3 the next, etc.

nullmod = glm(y ~ resp.factor + cat.factor, family = poisson)
glmod = glm( y ~ resp.factor + cat.factor + cat.factor:rnincome, 
			 family = poisson )
deviance(glmod)
# same as multinomial model above:
deviance(mmodi)
summary(glmod)	
# compare final coefficients for I, R, D, +income interactions with:
summary(mmodi)



# 5.2 Hierarchical or Nested Responses
data(cns)

cns$CNS = cns$An + cns$Sp + cns$Other
plot(log(CNS / NoCNS) ~ Water, cns, pch = as.character(Work))	
# Note outlier = 10: Newport

binmodw = glm(cbind(CNS, NoCNS) ~ Water + Work, cns, family = binomial)
binmoda = glm(cbind(CNS, NoCNS) ~ Area + Work, cns, family = binomial)
anova(binmodw, binmoda, test = "Chi")

halfnorm(resid(binmodw))		# again 10 (Newport) is an outlier

summary(binmodw)
exp(-0.339) # coef. for work = NonManual; 
			# defects for non-manual workers = 71.25% that for Manual

cmmod = multinom(cbind(An, Sp, Other) ~ Water + Work, cns)
nmod = step(cmmod)
summary(nmod)
cc = c(0, 0.2896, -0.9808)	# fitted proportions ( = coefs for ea)
names(cc) = c("An", "Sp", "Other")
exp(cc) / sum(exp(cc))
# Note: although water and labor type were significant for CNS vs NoCNS, they are not signifant predictors for type of CNS malformation (neither remains in the simplified model)

multinom(cbind(NoCNS, An, Sp, Other) ~ Water + Work, cns)



# 5.3 Ordinal Multinomial Responses
pomod = polr(sPID~age + educ + nincome, nes96)
c(deviance(pomod), pomod$edf)	#edf = no. of parameters estimated
c(deviance(mmod), mmod$edf)
pomodi = step(pomod)
deviance(pomodi)-deviance(pomod)
pchisq(11.151, pomod$edf-pomodi$edf, lower = F) # not sig. justifies using the 
												# simpler model
pim = prop.table(table(nincome, sPID),1)
logit(pim[,1]) - logit(pim[,1] + pim[,2])
summary(pomodi)	# odds of moving from D to I/R, or from D/I to R increases by a 
				# factor of exp(0.01312) = 1.0132 as income increases by one 
				# unit ( = $1000)
# for income = $0: 
ilogit(0.2091)	# = p(Democrat)
ilogit(1.2916) - ilogit(0.2091)	# = p(Independent)
predict(pomodi, data.frame(nincome = il, row.names = il), type = "probs")
x = seq(-4, 4, 0.05)
plot(x, dlogis(x), type = "l")
abline(v = c(0.209, 1.292)) # coefs from pomodi; divides prob from L to R as D/
							# I/R for income = $0
abline(v = c(0.209, 1.292) - 50*0.01312, col = "red")	# shows same divisions 
														# for income = $50,000 
														# (50 income units)
abline(v = c(0.209, 1.292) - 100*0.01312, col = "blue")	# and for income = 
														# $100,000


# Proportional Odds Model
pomod = polr(sPID ~ age + educ + nincome, nes96)
c(deviance(pomod), pomod$edf)	# dev, n. params
# compare to the multinomial logit model:
c(deviance(mmod), mmod$edf)

pomodi = step(pomod, direction = 'both')
deviance(pomodi) - deviance(pomod)	# 11.15
pchisq(11.151, pomod$edf - pomodi$edf, lower = F)	# p = 0.132; simplification
													# is justifiable
pim = prop.table(table(nincome, sPID), 1)
logit(pim[, 1]) - logit(pim[, 1] + pim[, 2])
# unclear if values are "constant", but at least no obeservable trend

summary(pomodi)
# The odds of changing from Dem -> Ind/Rep or Dem/Ind -> Rep increase by a factor of exp(0.013), or 1.013, for every unit ($1000) increase in income; based on the intercepts, the prob of being a Dem given $0 income is:
ilogit(0.209)	# 0.55
# ...and of being Independent:
ilogit(1.292) - ilogit(0.209) # 0.23
# ...and Repub
1 - 0.55 - 0.23	# 0.22

# Predicted values
predict(pomodi, data.frame(nincome = il, row.names = il), type = 'probs')

# Note prob of being independent increases, then decreases with increasing income, expected in a latent variable representation; illustrate with cutpoints for incomes at 0, 50k, and 100k:
x = seq(-4, 4, 0.05)
plot(x, dlogis(x), type = 'l')
abline(v = c(0.209, 1.292), col = 2)
abline(v = c(0.209, 1.292) - 50 * 0.013120, col = 4)
abline(v = c(0.209, 1.292) - 100 * 0.013120, col = 5)

# Ordered Probit Model
opmod = polr(sPID ~ nincome, method = "probit")
summary(opmod)	# both AIC and res.dev. better than pomodi
dems = pnorm(0.128 - il * 0.008182)	# coefs for D|I and nincome
demind = pnorm(0.798 - il * 0.008182)	# coefs for I|R and nincome
cbind(dems, inds = demind - dems, reps = 1 - demind)	# predicted %s of each 
														# at each income level 
														# (il--in $1000s)
# Proportional Hazards Model
phmod = polr(sPID ~ nincome, method = 'cloglog')
summary(phmod)


save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')