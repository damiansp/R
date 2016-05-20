#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===========================#
#							#
#	4.	Contingency Tables	#
#							#
#===========================#
rm(list=ls())
library(faraway)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')



# 4.1 Two-by-Two Tables
# Given: the following data:

# Quality	No Particles		Particles	Total
# Good		320				14			334
# Bad		80				36			116
# Total		400				50			450

# Options:
# Observe process over a time interval; observe 450 wafers; cross classify; model as Poisson
# Sample 450 wafers; cross classify; model as multinomial
# Select 400 wafers w/o particles and 50 w; record good or bad; model as binomial
# Select 400 wafers w/o particles and 50 w; by design, includes 334 good wafers and 116 bad; model as hypergeometric 

y <- c(320, 14, 80, 36)
particle <- gl(2, 1, 4, labels=c('no', 'yes'))
quality <- gl(2, 2, labels=c('good', 'bad'))
wafer <- data.frame(y, particle, quality)

(ov <- xtabs(y ~ quality + particle))


# Poisson Model
modl <- glm(y ~ particle + quality, family=poisson)
summary(modl)
par(mfrow=c(2, 2))
plot(modl)

drop1(modl, test='Chi')


# Multinomial Model
(pp <- prop.table(xtabs(y ~ particle)))
(qp <- prop.table(xtabs(y ~ quality)))
(fv <- outer(qp, pp) * 450)

# Deviance is:
2 * sum(ov * log(ov / fv))	# same as modl (Poisson) above


# Binomial Model
(m <- matrix(y, nrow=2))
colnames(m) <- c('q.good', 'q.bad')
rownames(m) <- c('p.no', 'p.yes')
m
modb <- glm(m ~ 1, family=binomial)
summary(modb)


# Hypergeometric
ov
fisher.test(ov)
# CI is for the odds ratio
# Odds ratio is:
(ov[1, 1] * ov[2, 2]) / (ov[1, 2] * ov[2, 1])



# 4.2 Larger Two-Way Tables
data(haireye)
haireye
(ct <- xtabs(y ~ hair + eye, haireye))

summary(ct)
dotchart(ct)

plot(ct, col=T, main='', las=1)

modc <- glm(y ~ hair + eye, family=poisson, data=haireye)
summary(modc)	# Not particularly interesting since we want to know about 
				# interactions

(z <- xtabs(resid(modc, type='pearson') ~ hair + eye, data=haireye))
svdz <- svd(z, 2, 2)		# Single value decomposition
leftsv <- svdz$u %*% diag(sqrt(svdz$d[1:2]))
rightsv <- svdz$v %*% diag(sqrt(svdz$d[1:2]))
ll <- 1.1 * max(abs(rightsv), abs(leftsv))
plot( rbind(leftsv, rightsv), asp=1, xlim=c(-3, 3), ylim=c(-3, 3), 
	  xlab='SV1', ylab='SV2 ', type='n')
abline(h=0, v=0, col='grey')
text(leftsv, dimnames(z)[[1]])
text(rightsv, dimnames(z)[[2]], col=2)
# Distance of a point from the origin is proportional to how 'atypical' its distribution is relative to the overall average; 



# 4.3 Matched Pairs
data(eyegrade)	# performance of left and right eye exams (paired for subjects)
head(eyegrade)
(ct <- xtabs(y ~ right + left, eyegrade))
summary(ct)

# Dependency is not surprising here (people with good/bad right eyes tend to have good/bad left eyes).  But what about data symmetry? I.e., is p[i, j] = p[j, i]? E.g., are people with good right and bad left eyes equally likely as those with good left and bad right?
(symfac <- factor(apply( eyegrade[, 2:3], 1, 
						 function(x) { 
							paste(sort(x), collapse='-')
		   				 } )))
cbind(eyegrade$y, symfac)
mods <- glm(y ~ symfac, eyegrade, family=poisson)
summary(mods)
c(deviance(mods), df.residual(mods))
pchisq(deviance(mods), df.residual(mods), lower=F) 

# This is evidence of a lack of symmetry--investigate residuals:
round(xtabs(resid(mods) ~ right + left, eyegrade), 3)
# Above-diag resids are mostly positive, and below, mostly negative--so more common to have a good right and poor left than vice versa
# Compute margins:
margin.table(ct, 1)
margin.table(ct, 2)
# Marginal values slightly heterogeneous (more good rights and poor lefts)
# Use quasi-symmetry model:
# Set p[i, j] = alpha[i]beta[j]gamma[i,j]; gamma[i, j] = gamma[j, i]
# logE(Y[i, j]) = lognp[i, j] = logn + logalpha[i] + logbeta[j] + loggamma[i, h]
modq <- glm(y ~ right + left + symfac, eyegrade, family=poisson)
summary(modq)
pchisq(deviance(modq), df.residual(modq), lower=F) 

# Model fits, indicating quasi-symmetry
# if marginal homogeneity + quasi symm -> symm
# Test for marginal homgeneity (compare symm to quasi-symm models):
anova(mods, modq, test='Chi')
# Differ signif. indicating a lake of marginal homogeneity
# Check for independence between eyes for people w/o symmetric vision: (quasi-independence hypothesis)
modqi <- glm( y ~ right + left, eyegrade, family=poisson, 
			  subset=-c(1, 6, 11, 16) )
summary(modqi)
pchisq(deviance(modqi), df.residual(modqi), lower=F) 
# Model does not fit.  The difference between eyes is likely to be smaller than expected under independence (notice, e.g., the farther the numbers from the diagonal [the greater the difference in the eyes], the fewer the numbers from the sample)



# 4.4 Three-Way Contingency Tables
data(femsmoke)
head(femsmoke)
(ct <- xtabs(y ~ smoker + dead, femsmoke))

prop.table(ct, 1)
(ct3 <- (xtabs(y ~ smoker + dead + age, femsmoke)))
(ct3p <- prop.table(ct3, c(1, 3)))
summary(ct)
(cta <- xtabs(y ~ smoker + dead, femsmoke, subset=(age == '55-64')))
prop.table(cta, 1)

prop.table(xtabs(y ~ smoker + age, femsmoke), 2)
# Note the much higher prevalence of smokers in the younger age classes

fisher.test(cta)
apply( ct3, 3, function(x) { 	
					(x[1, 1] * x[2, 2]) / (x[1, 2] * x[2, 1]) 
			   } )

mantelhaen.test(ct3, exact=T)


# Mutual Independence
# If all 3 vars independent -> p[i, j, k] = p[i]p[j]p[k]
# And since E(Y[i, j, k]) = np[i, j, k] ->
#	log(E(Y[i, j, k])) = logn + logp[i] + logp[j] + logp[k]
# This will be the null model
summary(ct3)		# clearly independence does not hold
# As a linear model:
modi <- glm(y ~ smoker + dead + age, femsmoke, family=poisson)
summary(modi) # a poorly fitting model
# The coefs are proportional to the marginals, so we don't really learn anything from this model


# Joint Independence
# p[i, j, k] = p[i, j]p[k] ->
#	logE(Y[i, j, k]) = logn + logp[i, j] + logp[k]
modj <- glm(y ~ smoker*dead + age, femsmoke, family=poisson) 
# indepence between smoker and dead, but jointly independent of age
summary(modj) #better than mutual independence but dev/df still high -> poor fit


# Conditional Independence
# p[(i, j) | k] = p[i | k]p[j | k] ->
# p[i, j, k] = p[i, k]p[j, k] / p[k] ->
#	logE(Y[i, j, k]) = logn + logp[i, k] + logp[j, k] - logp[k]
modc <- glm(y ~ smoker*age + dead*age, femsmoke, family=poisson) 
# smoking and dead independant given age
summary(modc)
# fairly good fit


# Uniform Association
# Consider all 2-way interactions
#	logE(Y[i, j, k]) = logn + logp[i] + logp[j]  + logp[k] + logp[i, j] + 
#		logp[i, k] + logp[j, k]

modu <- glm(y ~ (smoker+age+dead)^2, femsmoke, family=poisson) 
# tests all 2-way interactions
summary(modu)

# compute odds ratio for each age class
ctf <- xtabs(fitted(modu) ~ smoker+dead+age, femsmoke)
apply(ctf, 3, function(x)  { (x[1,1]*x[2,2]) / (x[1,2]*x[2,1]) })
# all odds are the same: for any level of one var, the association between the other 2 vars is the same
exp(coef(modu)['smokerno:deadno']) 
# extract log-odds ratio for smoking against dead for a given age group:
#	(E(Y[1,1,k])*E(Y[2,2,k])) / (E(Y[1,2,k])*E(Y[2,1,k])) where k = age

# Model Selection
modsat <- glm(y ~ smoker*age*dead, femsmoke, family=poisson)
summary(modsat)
drop1(modsat, test="Chi") # can drop 3-way interaction (= modu)
summary(modu)
drop1(modu, test="Chi")	# all interactions significant
plot(modu)


# Binomial Model
# Where one var may be seen as a response, and other 2 as predictor
ybin <- matrix(femsmoke$y, ncol=2) 	# col1 = dead.yes, col2 = dead.no; ea row is 
									# an age category
modbin <- glm(ybin ~ smoker * age, femsmoke[1:14,], family=binomial)
summary(modbin)

drop1(modbin, test="Chi")	# can drop interaction term
modbin <- glm(ybin ~ smoker + age, femsmoke[1:14,], family=binomial)
summary(modbin)
drop1(modbin, test="Chi")	# no further reduction
par(mfrow=c(2, 2))
plot(modbin)

deviance(modu); deviance(modbin)		# models have same predictions
exp(-coef(modbin)[2])	# again extracts odds ratio
# but binomial model implicitly assumes association between smoker and age (in the ybin response term)



#4.5 Ordinal Variables
data(nes96)
xtabs(~PID + educ, nes96)	# note obvious gradient on each dimension
(partyed <- as.data.frame.table(xtabs(~ PID + educ, nes96)))
nomod <- glm(Freq ~ PID + educ, partyed, family=poisson)
summary(nomod)

pchisq(deviance(nomod), df.residual(nomod), lower=F)
partyed$oPID <- unclass(partyed$PID)		# assigns numerical ordinal value
partyed$oeduc <- unclass(partyed$educ)

ormod <- glm(Freq ~ PID + educ + I(oPID * oeduc), partyed, family=poisson)
anova(nomod, ormod, test="Chi")
summary(ormod)	# estimate of gamma is est of I(oPID * oeduc) = 0.029; given assigments to variables, indicates higher educ --> greater prob of being republican

# check for robustness based on how levels are assigned:
levels(partyed$PID)
apid <- c(1, 2, 5, 6, 7, 10, 11)		# assign greater gap between party 
									# affiliations
levels(partyed$educ)
aedu <- c(1, 1, 1, 2, 2, 3, 3, 3)	# clump difft. education levels
ormoda <- glm( Freq ~ PID + educ + I(apid[oPID] * aedu[oeduc]), partyed, 
			   family=poisson )
anova(nomod, ormoda, test="Chi")
summary(ormoda)

# check for more structure than model assumes:
round(xtabs(resid(ormod, type="response") ~ PID + educ, partyed), 2) 
# note high val for weakRep:Coll and compar low for weakRep:MA, indicating non-monotone relationship between party and educ, suggests treating educ as nominal instead of ordinal:
cmod <- glm(Freq ~ PID + educ + educ:oPID, partyed, family=poisson)	
# compare coefs for educ:oPIDs (educMAdeg:oPID = NA --> = 0): if monotonic relationship in ed, coefs should also be monotone (not so); compare with linear-by-linear model:
anova(ormod, cmod, test="Chi") #simpler linear-by-linear mod preferred
summary(cmod)







save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')