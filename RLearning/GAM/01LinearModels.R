#=========#=========#=========#=========#=========#=========#=========#=========
#===================================#
#									#
#	Generalized Additive Models		#
#		An Introduction with R		#
#									#
#	Simon N. Wood. 2006				#
#									#
#===================================#

#=======================#
#						#
#	1. Linear Models	#
#						#
#=======================#
rm(list = ls())
setwd('~/Learning/R/RLearning/GAM')

library(gamair)
data(hubble)
data(sperm.comp1)
data(sperm.comp2)


# 1 A Simple Linear Model
# 1.2 So How Old is the Universe?
hub.mod <- lm(y ~ x - 1, data=hubble)
summary(hub.mod)
plot(y ~ x, hubble)
abline(hub.mod)
	
par(mfrow=c(2, 2))
plot(hub.mod)
	
hub.mod1 <- lm(y ~ x - 1, data=hubble[-c(3, 15), ])
summary(hub.mod1)
par(mfrow=c(1, 1))
plot(y ~ x, hubble, pch=16, col=rgb(0, 0 , 0, 0.5))
abline(hub.mod, col='grey')
abline(hub.mod1)
	
hubble.const <- c(coef(hub.mod), coef(hub.mod1)) / 3.09e19
ageSec <- 1 / hubble.const
(ageYear <- ageSec / (60^2 * 24 * 365))
	
# 3 Adding a Distributional Assumption
cs.hubble <- 163000000	# necessary hubble const. for biblical interp.
t.stat = (coef(hub.mod1) - cs.hubble) / summary(hub.mod1)$coefficients[2]
pt(t.stat, df = 21) * 2
	
# Confidence Intervals
qt(c(0.025, 0.975), df = 21) # range of t values consistent with null h.
	
sigb <- summary(hub.mod1)$coef[2]
(h.ci <- coef(hub.mod1) + qt(c(0.025, 0.975), df = 21) * sigb)
# Express as age of universe:
h.ci <- h.ci * 60 * 60 * 24 * 365.25 / 3.09e19 # = 1/yrs
sort(1 / h.ci)



# 5 Practical Linear Modeling
# 5.1 Model Fitting and Model Checking
pairs(sperm.comp1[, -1], panel=panel.smooth)
sc.mod1 <- lm(count ~ time.ipc + prop.partner, sperm.comp1)
model.matrix(sc.mod1)
summary(sc.mod1)
par(mfrow=c(2, 2))
plot(sc.mod1)
sperm.comp1[9, ]
	
# Try modeling hours together in place of prop. of time together
sc.mod2 <- lm(count ~ time.ipc + I(prop.partner * time.ipc), sperm.comp1)
summary(sc.mod2)
plot(sc.mod2)
sperm.comp1[4, ]

# 5.2 Model summary()
summary(sc.mod1)
	
# 5.3 Model selection
sc.mod3 <- step(sc.mod1)
summary(sc.mod3)
sc.mod3 <- update(sc.mod1, ~ . -time.ipc)
summary(sc.mod3)
	
sc.mod4 <- lm(count ~ 1, sperm.comp1)
AIC(sc.mod1, sc.mod2, sc.mod3, sc.mod4)
summary(sc.mod2)
par(mfrow=c(2, 2))
plot(sc.mod2)
	
# 5.4 Another model selection example
sc2.mod1 <- lm(count ~ f.age + f.height + f.weight + m.age + m.height 
                 + m.weight + m.vol, sperm.comp2)
summary(sc2.mod1)
par(mfrow=c(2, 2))
plot(sc2.mod1)
sperm.comp2[19, ]
sc2.mod2 <- update(sc2.mod1, ~ . -m.age)
summary(sc2.mod2)
# ...continue single-predictor simplifications...
sc2.mod7 <- lm(count ~ f.weight, sperm.comp2)
summary(sc2.mod7)
	
# Try again with outlier point 19 removed
sc3.mod1 <- lm(count ~ f.age + f.height + f.weight + m.age + m.height + m.weight  
                 + m.vol, sperm.comp2[-19, ])
summary(sc3.mod1)
sc3.mod2 <- step(sc3.mod1)
summary(sc3.mod2)
	
# A follow-up
sperm.comp1$m.vol <- sperm.comp2$m.vol[
  sperm.comp2$pair %in% sperm.comp1$subject]
sc1.mod1 <- lm(count ~ m.vol, sperm.comp1)
summary(sc1.mod1)
	
# 5.5 Confidence Intervals
sc.c <- summary(sc1.mod1)$coeff
sc.c
(sc.c.95ci <- sc.c[2, 1] + qt(c(0.025, 0.975), 6) * sc.c[2, 2])
	
# 5.6 Prediction
new <- c(10, 15, 20, 25)
df <- data.frame(m.vol=new)
(preds <- predict(sc1.mod1, df, se=T))
par(mfrow=c(1, 1))
plot(preds$fit ~ new, type='l', ylim=c(50, 700))
lines(preds$fit + (1.96 * preds$se.fit) ~ new, col=2)	
lines(preds$fit - (1.96 * preds$se.fit) ~ new, col=2)	

# 5.7 Colinearity, confounding and causation
n <- 100
x <- runif(n)
z <- x + 0.05*rnorm(n)
y <- 3*x + 2 + rnorm(n)
mod <- lm(y ~ z)
plot(y ~ z)
abline(mod, col=2)
summary(mod)

mod2 <- lm(y ~ x + z)
summary(mod2)
	
# 1.6 Pratical Modeling with Factors

	# 1.6.4 Using Factor Variables in R
	z = c(1, 1, 1, 2, 2, 1, 3, 3, 3, 3, 4)
	z = as.factor(z)
	
	PlantGrowth$group
	pgm.1 = lm(weight ~ group, data = PlantGrowth)
	par(mfrow = c(2, 2))
	plot(pgm.1)
	summary(pgm.1)
	
	pgm.0 = lm(weight ~ 1, data = PlantGrowth)
	anova(pgm.0, pgm.1)
	
	

# 1.7 General Linear Model Specifications in R
# y ~ a*b + x:z + offset(v) -1 # offset(v) forces v's coef to be 1
	
		
	
save.image('~/Desktop/R/GAM/GAM.RData')