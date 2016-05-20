#===============================================#
#												#
#	Nonparametric Statistical Methods Using R	#
#		Kloke & McKean (2015)					#
#												#
#								#===============#
#								#
#	Ch 3: Two-Sample Problems	#
#								#
#===============================#
rm(list=ls())
library(HSAUR2)
library(npsm)
library(Rfit)
#data(esoph)
#data(USmelanoma)
#data(sievers)
load('~/Desktop/R/NonparametricStats/NPS.RData')


# 3.1 Introductory Example
# Ex. 3.1.1 Malignant Melanoma
head(USmelanoma)



# 3.2 Rank-Based Analyses
x <- rep(esoph$alcgp, esoph$ncases)
y <- rep(esoph$alcgp, esoph$ncontrols)
z <- c(x, y)
w <- c(rep(1, length(x)), rep(0, length(y)))
barplot( table(z, w), names.arg=c('Cases', 'Controls'), 
		 legend.text=levels(esoph$alcgp) )

x <- as.numeric(x)
y <- as.numeric(y)
wilcox.test(x, y)
	
	# 3.2.2 Analyses for a Shift in Location
	z <- c(12, 18, 11, 5, 11, 5, 11, 11)
	rank(z)
	
	# Ex. 3.2.2 Generated t[5]-Data
	# True shift = 8:
	x <- round(rt(50, 5) * 10 + 42, 2)
	y <- round(rt(60, 5) * 10 + 50, 2)
	sort(x)
	sort(y)
	wilcox.test(x, y, exact=T)
	
	rank.test(x, y)
	
	wilcox.test(x, y, conf.int=T, conf.level=0.99)
	
	# Normal scores
	rank.test(x, y, scores=nscores, conf.int=T, conf.level=0.95)
	
	
	# 3.2.3 Analyses Based on General Score Functions
	
	# 3.2.4 Linear Regression Model
	# Let Z = [X[1], ..., X[n1], Y[1], ..., Y[n2]]^T and
	# c is an n x 1 vector with the ith val = 0 if 1 ≤ i ≤ n1 and 1 if
	# n1 + 1 ≤ i ≤ n; 	(n = n1 + n2); then the location model is:
	# Z[i] = alpha + c[i]delta + e[i], with e[i]s iid with pdf f(t), and 
	# delta estimated with a regression model (Y.bar - X.bar if least squares)
	z <- c(x, y)
	ci <- c(rep(0, length(x)), rep(1, length(y)))
	fitns <- rfit(z ~ ci, scores=nscores)
	summary(fitns)
	
	# Exa. 3.2.3 Quail Data
	head(quail2)
	boxplot(ldl ~ treat, data=quail2)
	fit <- rfit(ldl ~ treat, data=quail2)
	summary(fit)
	lmfit <- lm(ldl ~ treat, data=quail2)
	summary(lmfit)
	
	fitns <- rfit(ldl ~ treat, data=quail2, scores=nscores)
	summary(fitns)
	# note the lm (least squares) model is more affected by the outlier, 
	# hence the Wilcoxon (rfit), and normal scores (scores=nscores) methods 
	# are more robust
	


# 3.3 Scale Problems
args(fk.test)

	# Ex. 3.3.1 Effect of Ozone on Rat Weights
	x <- with(sievers, weight.gain[group == 'Control'])
	y <- with(sievers, weight.gain[group == 'Ozone'])
	fk.test(x, y)
	boxplot(weight.gain ~ group, data=sievers)
	# p = 0.036, indicates sig. diff. in variance of groups
	
	# Cf. f-test (assumes normality):
	var.test(x, y)
	
	

# 3.4 Placement Test for the Behrens-Fisher Problem
# Given two samples with different variances, do the locations (e.g., medians) differ? (cf. Welch's t-test)
	# Ex. 3.4.1 Geese
	# plasma glucose levels in:
	lg <- c(293, 291, 289, 430, 510, 353, 318)	# lead-poisoned geese and:
	hg <- c(227, 250, 277, 290, 297, 325, 337, 340)	# healthy geese
	# Hypothesis is that lg > hg:
	fp.test(hg, lg, alternative='greater')
	boxplot(lg, at=1, xlim=c(0.5, 2.5), ylim=range(c(lg, hg)))
	boxplot(hg, at=2, add=T)
	axis(1, at=c(1, 2), labels=c('lg', 'hg'))
	


# 3.5 Efficiency and Optimal Scores
	# 3.5.1 Efficiency
	
	
	
# 3.6 Adaptive Rank Scores Test
	# Ex. 3.6.1 
	m <- 50 
	n <- 55
	# Exponential
	hogg.test(rexp(m), rexp(n))

	# Cauchy	
	hogg.test(rcauchy(m), rcauchy(n))
	
	# Normal
	hogg.test(rnorm(m), rnorm(n))
	
	# Uniform
	hogg.test(runif(m), runif(n))
	
	

save.image('~/Desktop/R/NonparametricStats/NPS.RData')