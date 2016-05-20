#=======================================================#
#														#
#	XML and Web Technologies for Data Sciences with R	#
#		Nolan & Lang (2014)								#
#														#
#							#===========================#
#							#
#	2. Introduction to XML	#
#							#
#===========================#
rm(list=ls())
library(pmml)

load('~/Desktop/R/WebTech/webTech.RData')

w = rnorm(10000, 0, 2)
x = rnorm(10000, 0, 2)
z = rnorm(10000, 0, 2)
y = 3*x + 2*x*z + z + w

mod = lm(y ~ x + I(x^2) + x:z + z + I(z^2))
mod = step(mod, direction = 'both')
summary(mod)
par(mfrow = c(2, 2))
plot(mod)

saveXML(pmml(mod), file = '~/Desktop/R/WebTech/mod.pmml')













save.image('~/Desktop/R/WebTech/webTech.RData')
