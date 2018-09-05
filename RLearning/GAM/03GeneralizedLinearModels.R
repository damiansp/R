#=========#=========#=========#=========#=========#=========#=========#=========
#===================================#
#                                   #
#   Generalized Additive Models     #
#      An Introduction with R       #
#                                   #
#      Simon N. Wood. 2006          #
#                                   #
#                                   #
#                                   #
#   3. Generalized Linear Models    #
#                                   #
#===================================#
rm(list = ls())
setwd('~/Learning/R/RLearning/GAM')
library(gamair)
data(stomata)



# 2 Geometry of GLMs
# 2.2 Geometry of IRLS convergence
x <- c(0.6, 1.5)
ms <- exp(-x * 4)
y <- c(0.02, 0.09)
glm(y ~ I(-x) - 1, family=gaussian(link=log), mustart=ms)
ms <- exp(-x * 0.1)
glm(y ~ I(-x) - 1, family=gaussian(link=log), mustart=ms)



# 3 GLMs with R
# 3.1 Binomial models and heart disease
  heart <- data.frame(
    ck = c(20, 60, 100, 140, 180, 220, 260, 300, 340, 380, 420, 460),
    ha = c( 2, 13,  30,  30,  21,  19,  18,  13,  19,  15,   7,   8),
    ok = c(88, 26,   8,   5,   0,   1,   1,   1,   1,   0,   0,   0)  
  )
  p <- heart$ha / (heart$ha + heart$ok)
  plot(heart$ck, p, xlab = 'Creatinine Kinase Level', 
       ylab = 'Proportion Heart Attacks')
  
  mod.0 <- glm(cbind(ha, ok) ~ ck, family = binomial(link = 'logit'), data = heart)
  summary(mod.0)
  # Deviance explained:
  1 - (36.929 / 217.712)
  
  par(mfrow = c(2, 2))
  plot(mod.0)
  1 - pchisq(36.929, df = 10) # also indicates poor fit based on prob of dev for df

  par(mfrow = c(1, 1))
  plot(heart$ck, p, xlab = 'Creatinine Kinase Level', 
       ylab = 'Proportion Heart Attacks')
  lines(heart$ck, fitted(mod.0), col = 2)
  
  # Try cubic model instead
  mod.2 <- glm(cbind(ha, ok) ~ ck + I(ck^2) + I(ck^3), family = binomial, 
  			   data = heart)  
  summary(mod.2)
  lines(heart$ck, fitted(mod.2), col = 4)
  
  par(mfrow = c(2, 2))
  plot(mod.2)
  1 - (4.2525 / 271.7124)  	# better
  1 - pchisq(4.2525, 8)		# much better
  
  
  # 2.3.2 A Poisson regression epidemic model
  y <- c(12, 14, 33, 50, 67, 74, 123, 141, 165, 204, 243, 246, 240)
  t <- 1:13
  plot(t + 1980, y, xlab = 'Year', ylab = 'New AIDS cases', ylim = c(0, 250))
  
  m0 <- glm(y ~ t, poisson)
  summary(m0)
  1 - pchisq(77.623, 11)
  par(mfrow = c(2, 2))
  plot(m0)
  
  m1 <- glm(y ~ t + I(t^2), poisson)
  summary(m1)
  1 - pchisq(7.5816, 10)
  plot(m1)
  
  anova(m0, m1, test = 'Chisq')
  
  (beta1 <- summary(m1)$coef[2, ])
  (ci <- c(beta1[1] - 1.96 * beta1[2], beta1[1] + 1.96 * beta1[2]))
  
  new.t <- seq(1, 13, length = 100)
  fv <- predict(m1, data.frame(t = new.t), se = T)
  par(mfrow = c(1, 1))
  plot(t + 1980, y, xlab = 'Year', ylab = 'New AIDS cases', ylim = c(0, 270))
  lines(new.t + 1980, exp(fv$fit), col = 2)
  lines(new.t + 1980, exp(fv$fit + 1.96 * fv$se.fit), col = 2, lty = 2)
  lines(new.t + 1980, exp(fv$fit - 1.96 * fv$se.fit), col = 2, lty = 2)


	# 2.3.3 Log-linear models for categorical data
	a1 <- data.frame(y = c(435, 147, 375, 134), 
					 gender = as.factor(c('F', 'F', 'M', 'M')),
					 faith = as.factor(c(1, 0, 1, 0)))
	a1
	mod.0 <- glm(y ~ gender + faith, data = a1, family = poisson)
	model.matrix(mod.0)
	summary(mod.0)
	1 - pchisq(0.162, 1)
	
	mod.1 <- update(mod.0, .~. + gender:faith)
	model.matrix(mod.1)
	summary(mod.1)
	
	anova(mod.0, mod.1, test = 'Chisq') # prefer simpler mod.0
	
	
	# 2.3.4 Sole eggs in the Bristol channel
	data(sole)
	head(sole)
	
	sole$off <- log(sole$a.1 - sole$a.0)
	sole$a <- (sole$a.1 + sole$a.0) / 2
	solr <- sole
	solr$t <- solr$t - mean(sole$t)
	solr$t <- solr$t / var(sole$t)^0.5
	solr$la <- solr$la - mean(sole$la)
	solr$lo <- solr$lo - mean(sole$lo)
	
	b <- glm(eggs ~ offset(off) + lo + la + t + I(lo * la) + I(lo^2) + I(la^2) +
			 I(t^2) + I(lo * t) + I(la * t) + I(lo^3) + I(la^3) + I(t^3) + 
			 I(lo * la * t) + I(lo^2 * la) + I(lo * la^2) + I(lo^2 * t) + 
			 I(la^2 * t) + I(la * t^2) + I(lo * t^2) + a + I (a * t) + I(t^2 * a),
			 family = quasi(link = log, variance = 'mu'), data = solr)
	summary(b)
	
	b1 <- update(b, .~. -I(lo * t))
	summary(b1)
	b2 <- update(b1, .~. -I(lo * la * t))
	summary(b2)
	b3 <- update(b2, .~. -I(lo * t^2))
	summary(b3)
	b4 <- update(b3, .~. -I(lo^2 * t))
	summary(b4)
	
	anova(b, b4, test = 'F') # use simpler mod, b4
	
	par(mfrow = c(1, 2))
	plot(sqrt(fitted(b4)), sqrt(solr$eggs)) # fitted vs. actual
	plot(sqrt(fitted(b4)), resid(b4)) # resid vs root fitted
	


save.image('~/Desktop/R/GAM/GAM.RData')