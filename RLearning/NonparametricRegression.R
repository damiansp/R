# Nonparametric linear models
# Source: 
#	http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-nonparametric-regression.pdf
#	Fox, John. 2002. Appendix to "An R and S-PLUS Companion to Applied Regression"

# 2.1 Local Polynomial Regression
library(car)
data(Prestige)
plot(Prestige$income, Prestige$prestige, xlab='Avg Income', ylab='Prestige')
lines(lowess(Prestige$income, Prestige$prestige, f=0.5, iter=0), col=2) 
# f is the span of the smoother; iter=0 means local regression should NOT be refit to downweight
# outlying observations

	# 2.1.2 Multiple Regression
	#library(modreg)	# no longer needed, merged into stats package
	mod.lo  <- loess(prestige ~ income + education, data=Prestige, span=0.5, degree=1L)
	# degree is the degree of the local polynomial
	summary(mod.lo)
	
	# To see/visualize the fitted model:
	inc <- seq(min(Prestige$income), max(Prestige$income), len=50)
	ed <- seq(min(Prestige$education), max(Prestige$education), len=50)
	newdata <- expand.grid(income=inc, education=ed)
	fit.prestige <- matrix(predict(mod.lo, newdata), 50, 50)
	persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype='detailed', xlab='Income', 
		  ylab='Education', zlab='Prestige', expand=2/3, shade=0.4)
		  
	# Assess statistical significance of each predictor by dropping it from the model and 
	# performing an approximate incremental F-test for the change in RSS
	# Note: for the separate models, span is update to 0.7 or appx sqrt(0.5)
	mod.lo.inc <- loess(prestige ~ income, data=Prestige, span=0.7, degree=1) # omit ed
	mod.lo.ed <- loess(prestige ~ education, data=Prestige, span=0.7, degree=1) # omit income
	
	anova(mod.lo.inc, mod.lo)
	
	anova(mod.lo.ed, mod.lo)
