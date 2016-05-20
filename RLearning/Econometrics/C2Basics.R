#===============#
#               #
#	2 Basics	#
#               #
#===============#
rm(list = ls())
library('AER')
library('quantreg')
library('KernSmooth')
load('~/Desktop/R/Econometrics/Econometrics.RData')

# 2.7  R Graphics
data('Journals')
head(Journals)

Journals$citeprice = Journals$price / Journals$citations
attach(Journals)

plot(log(subs), log(citeprice))
rug(log(subs))
rug(log(citeprice), side = 2)

detach(Journals)

plot(log(subs) ~ log(citeprice), data = Journals, pch = 20, col = 'blue', 
	 ylim = c(0, 8), xlim = c(-7, 4), main = 'Library Subscriptions')

curve(dnorm, from = -4.5, to = 4.5, col = 'slategrey', lwd = 3, 
  	  main = 'Density of the Standard Normal Distribution')
text(-4.5, 0.3, 
	 expression(f(x) == frac(1, sigma ~~ sqrt(2*pi)) ~~ 
	 			e^{-frac((x - mu)^2, 2*sigma^2)}), adj = 0)
text(2, 0.3, 
	 expression(f(x) == frac(1, sigma*sqrt(2*pi)) * 
	 			e^{-frac((x - mu)^2, 2*sigma^2)}), adj = 0)



# 2.8 Exploratory Data Analysis with R
data('CPS1985')
str(CPS1985)
head(CPS1985)

levels(CPS1985$occupation)[c(2, 6)] = c('tech', 'mgmt')
attach(CPS1985)

summary(wage)
hist(wage, freq = F)
hist(log(wage), freq = F)
lines(density(log(wage)), col = 4)
rug(log(wage))

# One Categorical Variable
summary(occupation)
(tab = table(occupation))
prop.table(tab)

barplot(tab)
pie(tab)

# Two Categorical Variables
xtabs(~ gender + occupation)
plot(gender ~ occupation, col = c(4, 2))
abline(h = 0.5)

# Two Numerical Variables
cor(log(wage), education)
cor(log(wage), education, method = 'spearman') # for non-parametric
plot(log(wage) ~ education)

# One Numerical and One Categorical
tapply(log(wage), gender, mean)
tapply(log(wage), gender, median)
plot(log(wage) ~ gender, notch = T)

mwage = subset(CPS1985, gender == 'male')$wage
fwage = subset(CPS1985, gender == 'female')$wage
qqplot(mwage, fwage, xaxs = 'i', yaxs = 'i', xlim = c(0, max(mwage)),
	   ylim = c(0, max(fwage)))
abline(0, 1)

detach(CPS1985)



save.image('~/Desktop/R/Econometrics/Econometrics.RData')