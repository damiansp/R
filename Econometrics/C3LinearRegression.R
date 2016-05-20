#===========================#
#               			#
#	3 Linear Regression		#
#               			#
#===========================#
rm(list = ls())
#install.packages('plm', repos = 'http://cran.us.r-project.org', dependencies = T)
library(AER)
library(dynlm)
library(plm)
library(splines)
#library('quantreg')
#library('KernSmooth')
load('~/Desktop/R/Econometrics/Econometrics.RData')

# 3.1 Simple Linear Regression
data('Journals')
head(Journals)
journals <- Journals[, c('subs', 'price', 'citations')]
journals$citeprice <- Journals$price / Journals$citations
journals$pageprice <- Journals$price / Journals$pages
head(journals)

plot(citeprice ~ pageprice, journals)
plot(log(citeprice) ~ log(pageprice), journals)
plot(log(price) ~ log(citations), journals)
plot(log(price) ~ log(subs), journals)

jour.lm <- lm(log(subs) ~ log(citeprice), journals)
plot(log(subs) ~ log(citeprice), journals)
abline(jour.lm, col = 2)

names(jour.lm)
names(summary(jour.lm))

summary(jour.lm)
plot(subs ~ citeprice, journals)
xv <- seq(0.01, 25, length = 100)
yv <- exp(4.76621 - 0.53305*log(xv))
lines(yv ~ xv, col = 2)

# Analysis of Variance (ANOVA)
anova(jour.lm)

# Point and Interval Estimates
coef(jour.lm)
confint(jour.lm, level = 0.95)
predict(jour.lm, newdata = data.frame(citeprice = 2.11), 
		interval = 'confidence')
predict(jour.lm, newdata = data.frame(citeprice = 2.11), 
		interval = 'prediction')

lciteprice <- seq(-6, 4, 0.05)
jour.pred <- predict(jour.lm, interval = 'prediction', 
					newdata = data.frame(citeprice = exp(lciteprice)))
plot(log(subs) ~ log(citeprice), data = journals)
lines(jour.pred[, 1] ~ lciteprice, col = 2)
lines(jour.pred[, 2] ~ lciteprice, col = 2, lty = 2)
lines(jour.pred[, 3] ~ lciteprice, col = 2, lty = 2)

plot(subs ~ citeprice, journals)
lines(exp(jour.pred[, 1]) ~ exp(lciteprice), col = 2)
lines(exp(jour.pred[, 2]) ~ exp(lciteprice), col = 2, lty = 2)
lines(exp(jour.pred[, 3]) ~ exp(lciteprice), col = 2, lty = 2)

# Plotting lm Objects
par(mfrow = c(2, 2))
plot(jour.lm)

# Testing a Linear Hypothesis
linearHypothesis(jour.lm, 'log(citeprice) = -0.5')



# 3.2 Multiple Linear Regression
data(CPS1988)
head(CPS1988)
cps.lm <- lm(log(wage) ~ experience + I(experience^2) + education + ethnicity,
			 CPS1988)
summary(cps.lm)

# Comparison of Models
cps_noeth <- lm(log(wage) ~ experience + I(experience^2) + education, CPS1988)
anova(cps_noeth, cps.lm)

anova(cps.lm)

waldtest(cps.lm, . ~ . - ethnicity) # gives ANOVA results of original and updated



# 3.3 Partially Linear Models
cps.plm <- lm(log(wage) ~ bs(experience, df = 5) + education + ethnicity, CPS1988)
# bs() is B Splines non-parametric
# df for the bs() was chosen via BIC (Schwarz criterion) via:
cps.bs <- lapply(3:10, function(i) { 
  lm(log(wage) ~ bs(experience, df = i) + education + ethnicity, CPS1988)
})
structure(sapply(cps.bs, AIC, k = log(nrow(CPS1988))), .Names = 3:10) # min @ 5

cps <- data.frame(
  experience = -2:60, 
  education = with(CPS1988, mean(education[ethnicity == 'cauc'])),
  ethnicity = 'cauc'
)
cps$yhat1 <- predict(cps.lm, newdata = cps)
cps$yhat2 <- predict(cps.plm, newdata = cps)

plot(log(wage) ~ jitter(experience, factor = 3), pch = 16, 
	 col = rgb(0, 0, 1, 0.02), CPS1988)
lines(yhat1 ~ experience, data = cps, col = 2)
lines(yhat2 ~ experience, data = cps)

legend('topleft', c('quadratic', 'spline'), lty = 1, col = 2:1, bty = 'n')



# 3.4 Factors, Interactions, and Weights
cps.int <- lm(log(wage) ~ experience + I(experience^2) + education * ethnicity,
			  data = CPS1988)
summary(cps.int)

# Separate regressions for each level
cps.sep <- lm(log(wage) ~ ethnicity / (experience + I(experience^2) + education) 
			  -1, data = CPS1988)
summary(cps.sep)

anova(cps.sep, cps.lm)

# Change of the reference category
CPS1988$region <- relevel(CPS1988$region, ref = 'south')
cps.region <- lm(log(wage) ~ experience + I(experience^2) + education + ethnicity 
				 + region, data = CPS1988)
summary(cps.region)

# Weighted least squares
jour.wls <- lm(log(subs) ~ log(citeprice), data = journals, 
			   weights = 1 / citeprice^2)
jour.wls2 <- lm(log(subs) ~ log(citeprice), data = journals, 
			    weights = 1 / citeprice)

plot(log(subs) ~ log(citeprice), data = journals)
abline(jour.lm)
abline(jour.wls, col = 2)
abline(jour.wls2, col = 4)

auxreg <- lm(log(resid(jour.lm)^2) ~ log(citeprice), data = journals)
# feasible generalized least squares
jour.fgls <- lm(log(subs) ~ log(citeprice), weights = 1 / exp(fitted(auxreg)),
			    data = journals)
gamma2i <- coef(auxreg)[2]
gamma2 <- 0
while(abs((gamma2i - gamma2) / gamma2) > 1e-7) {
  gamma2 <- gamma2i
  fglsi <- lm(log(subs) ~ log(citeprice), data = journals,
			  weights = 1 / citeprice^gamma2)
  gamma2i <- coef(lm(log(resid(fglsi)^2) ~ log(citeprice), data = journals))[2]
}

jour.fgls2 <- lm(log(subs) ~ log(citeprice), data = journals, 
				 weights = 1 / citeprice^gamma2)
abline(jour.fgls2, col = 5)



# 3.5 Linear Regression with Time Series Data
data('USMacroG')
head(USMacroG)
str(USMacroG)
plot(USMacroG[, c('dpi', 'consumption')], col = 1:2, plot.type = 'single', 
	 ylab = '')
legend('topleft', legend = c('income', 'consumption'), lty = 1, col = 1:2, 	
	   bty = 'n')

#library(dynlm)
cons.lm1 <- dynlm(consumption ~ dpi + L(dpi), data = USMacroG)
cons.lm2 <- dynlm(consumption ~ dpi + L(consumption), data = USMacroG)
summary(cons.lm1)
summary(cons.lm2)

plot(
  merge(
    as.zoo(USMacroG[, 'consumption']), 
    fitted(cons.lm1), 
    fitted(cons.lm2), 
    0,
    resid(cons.lm1),
    resid(cons.lm2)
  ), 
  screens = rep(1:2, c(3, 3)),
  col = rep(1:3, 2),
  ylab = c('Fitted Values', 'Residuals'), xlab = 'Time', main = ''
)
legend(1950, 6700, c('observed', 'cons.lm1', 'cons.lm2'), lty = 1, col = 1:3, 
	   bty = 'n')
	   
# Encompassing Text
cons.lmE <- dynlm(consumption ~ dpi + L(dpi) + L(consumption), USMacroG)
anova(cons.lm1, cons.lmE, cons.lm2)
encomptest(cons.lm1, cons.lm2)



# 3.6 Linear Regression with Panel Data
# Static linear models
data('Grunfeld') # package = 'AER'
#library(plm)
gr <- subset(Grunfeld, firm %in% c('General Electric', 'General Motors','IBM'))
pgr <- plm.data(gr, index = c('firm', 'year'))
# OLS on pooled data
gr.pool <- plm(invest ~ value + capital, data = pgr, model = 'pooling')
summary(gr.pool)
# Fixed effects model
gr.fe <- plm(invest ~ value + capital, data = pgr, model = 'within')
summary(gr.fe)

# Are fixed effects needed?
pFtest(gr.fe, gr.pool)
# ...indicates signif variation between firms

# Random effects
gr.re <- plm(invest ~ value + capital, data = pgr, model = 'random', 
			 random.method = 'walhus')
summary(gr.re)

# Random effects needed?
plmtest(gr.pool)

# Test for paramater endogeneity
phtest(gr.re, gr.fe)
# ...does not appear to be a proble


# Dynamic linear models
data('EmplUK') # package = 'plm'
form <- log(emp) ~ log(wage) + log(capital) + log(output)
empl.ab <- pgmm(
  # lag.form = list(n.endogenous terms, lag.pred1, lag.pred2, lag.pred3)
  dynformula(form, lag.form = list(2, 1, 0, 1)), 
  data = EmplUK, index = c('firm', 'year'),
  effect = 'twoways', model = 'twosteps', gmm.inst = ~ log(emp), 
  lag.gmm = list(c(2, 99))
)
summary(empl.ab)





save.image('~/Desktop/R/Econometrics/Econometrics.RData')
