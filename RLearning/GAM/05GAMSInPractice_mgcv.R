#===================================#
#									#
#	Generalized Additive Models		#
#		An Introduction with R		#
#									#
#	Simon N. Wood. 2006				#
#									#
#===================================#

#===============================#
#								#
#	5. GAMS in Practice: mgcv	#
#								#
#===============================#
rm(list = ls())
#install.packages('gamair', repos = 'http://cran.us.r-project.org')
library(gamair)
library(mgcv)
load('~/Desktop/R/GAM/GAM.RData')

# 5.1 Cherry Trees Again
data(trees)
ct1 <- gam(Volume ~ s(Height) + s(Girth), family = Gamma(link = log), data = trees)

ct1
summary(ct1)
par(mfrow = c(1, 2))
plot(ct1, residuals = T, pch = 16, cex = 0.4)


  # 5.1.1 Finer Control of gam
  # Default smoother is thin plate regression splines, good but costly for large data 
  # sets; change to penalized cubic regression splines:
  ct2 <- gam(Volume ~ s(Height, bs = 'cr') + s(Girth, bs = 'cr'), 
  			 family = Gamma(link = log), 
  			 data = trees)
  summary(ct2)
  plot(ct2, residuals = T, pch = 16, cex = 0.4)
  
  # By default k = 10, which controls the upper bound of possible dfs allowed per 
  # parameter
  ct3 <- gam(Volume ~ s(Height) + s(Girth, bs = 'cr', k = 20), 
  			 family = Gamma(link = log), 
  			 data = trees)
  summary(ct3)
  plot(ct3, residuals = T, pch = 16, cex = 0.4)
  
  # GCV fitting param gamma defaults to 1; changing to 1.4 can help eliminate 
  # overfitting
  ct4 <- gam(Volume ~ s(Height) + s(Girth), 
  			 family = Gamma(link = log), 
  			 data = trees, 
  			 gamma = 1.4)
  summary(ct4)
  plot(ct4, residuals = T, pch = 16, cex = 0.4)
  
  
  # 5.1.2 Smooths of Several Variables
  ct5 <- gam(
    Volume ~ s(Height, Girth, k = 25), family = Gamma(link = log), data = trees)
  summary(ct5)
  plot(ct5, too.far = 0.15)

  ct6 <- gam(
    Volume ~ te(Height, Girth, k = 5), family = Gamma(link = log), data = trees)
  summary(ct6)
  plot(ct6, too.far = 0.2)
  # Note tensor te() model has both fewer edf and lower GCV
  
  
  # 5 1.3 Parametric Model Terms
  par(mfrow = c(1, 2))
  plot(ct1)
  # Replace (virtually straight-line est) s(Height) with a parametric predictor
  # But for fun, make it factor
  trees$hClass <- factor(floor(trees$Height / 10) - 5, 
  						 labels = c('small', 'medium', 'large'))
  ct7 <- gam(Volume ~ hClass + s(Girth), family = Gamma(link = log), data = trees)  
  par(mfrow = c(1, 2))
  plot(ct7, all.terms = T)
  anova(ct7)
  AIC(ct7)
  summary(ct7)
  
  
  
# 5.2  Brain Imaging Example
data(brain)

# Exclude outliers (n = 2)
brain <- brain[brain$medFPQ > 5e-3, ]
head(brain)

m0 <- gam(medFPQ ~ s(Y, X, k = 100), data = brain)
gam.check(m0) # medFPQ is highly skewed

# Assuming var(y[i]) prop. to µ[i]^beta; est beta as:
#lm(log(e^2) ~ log(fv)) # = 1.912; var increases with the square of the mean
m1 <- gam(medFPQ^0.25 ~ s(Y, X, k = 100), data = brain)
gam.check(m1)

#gm <- gam.method(gam = 'perf.magic') # gam.method not found
m2 <- gam(medFPQ ~ s(Y, X, k = 100), 
		  data = brain, 
		  family = Gamma(link = log))
gam.check(m2)

mean(brain$medFPQ)
mean(fitted(m1)^4)
mean(fitted(m2))	# this model substantially better

summary(m2)
vis.gam(m2, 
 		plot.type = 'contour', 
 		too.far = 0.03, 
 		ngrid = 60, 
 		zlim = c(-1, 2))

  # 5.2.2 Would an Additive Structure be Better?
  m3 <- gam(medFPQ ~ s(Y, k = 30) + s(X, k = 30), 
  			data = brain, 
  			family = Gamma(link = log))
  summary(m3)
  anova(m3, m2, test = 'F')
  
  m4 <- gam(medFPQ ~ s(Y, k = 30) + s(X, k = 30) + s(X, Y, k = 100), 
  			data = brain,
  			family = Gamma(link = log))
  summary(m4)
  
  vis.gam(m2, plot.type = 'contour', too.far = 0.03, n.grid = 60, zlim = c(-1, 2))
  
  # 5.2.3 Isotropic or Tensor Product Smooths?
  # Generally, tensor better if scales are very different
  tm <- gam(medFPQ ~ te(Y, X, k = 10), data = brain, family = Gamma(link = log))
  tm1 <- gam(medFPQ ~ s(Y, k = 10, bs = 'cr') + s(X, k = 10, bs = 'cr'),
  			 data = brain,
  			 family = Gamma(link = log))
  summary(tm1)
  summary(tm)
  
  anova(tm1, tm, test = 'F')
  
  # 5.2.4 Detecting Symmetry (with _by_ Variables)
  brain$Xc <- abs(brain$X - 64.5)
  brain$right <- as.numeric(brain$X < 64.5)
  m.sy <- gam(medFPQ ~ s(Y, Xc, k = 100), data = brain, family = Gamma(link = log))
  m.as <- gam(medFPQ ~ s(Y, Xc, k = 100) + s(Y, Xc, k = 100, by = right),
  			  data = brain,
  			  family = Gamma(link = log))
  anova(m.sy, m.as, test = 'F')
  
  vis.gam(m.sy, 
  		  plot.type = 'contour', 
  		  view = c('Xc', 'Y'),
  		  too.far = 0.03,
  		  n.grid = 60,
  		  zlim = c(-1, 2),
  		  main = 'Both sides')
  vis.gam(m.as, 
  		  plot.type = 'contour', 
  		  view = c('Xc', 'Y'),
  		  cond = list(right = 0),
  		  too.far = 0.03,
  		  n.grid = 60,
  		  zlim = c(-1, 2),
  		  main = 'Left side')
  vis.gam(m.as, 
  		  plot.type = 'contour', 
  		  view = c('Xc', 'Y'),
  		  cond = list(right = 1),
  		  too.far = 0.03,
  		  n.grid = 60,
  		  zlim = c(-1, 2),
  		  main = 'Right side')

  # 5.2.5 Comparing Two Surfaces
  brain1 <- brain
  mu <- fitted(m2)
  n <- length(mu)
  ind <- brain1$X < 60 & brain1$Y < 20
  mu[ind] <- mu[ind] / 3
  set.seed(1)
  brain1$medFPQ <- rgamma(rep(1, n), mu / m2$sig2, scale = m2$sig2)
  
  brain2 <- rbind(brain, brain1)
  brain2$sample1 <- c(rep(1, n), rep(0, n))
  brain2$sample0 <- 1 - brain2$sample1
  
  m.same <- gam(medFPQ ~ s(Y, X, k = 100), data = brain2, family = Gamma(link = log))
  m.diff <- gam(medFPQ ~ s(Y, X, k = 100) + s(Y, X, by = sample1, k = 100), 
  				data = brain2, 
  				family = Gamma(link = log))
  summary(m.same) 
  summary(m.diff) # lower GCV indicates better model, also:
  AIC(m.same)
  AIC(m.diff) # likewise
  anova(m.same, m.diff, test = 'F') # sig diff; prefer more complex (m.diff)
  
  # 5.2.6 Prediction with predict.gam
  # on scale of the linear predictor
  predict(m2)[1:5]
  pv <- predict(m2, se = T)
  pv$fit[1:5]
  pv$se[1:5]
  
  # on the response scale
  pv <- predict(m2, type = 'response', se = T)
  pv$fit[1:5]
  pv$se[1:5]
  
  # new hypothetical data
  pd <- data.frame(X = c(80.1, 68.3), Y = c(41.8, 41.8))
  predict(m2, newdata = pd, type = 'response', se = T)
  
  # contributions of each model term (excluding intercept)
  summary(m3)
  predict(m3, newdata = pd, type = 'terms', se = T)
  
  # Predictions with lpmatrix
  # Construct a model matrix, X s.t. Xbeta = eta (the linear predictor)
  Xp <- predict(m2, newdata = pd, type = 'lpmatrix')
  # Then use X to find values of the linear predictor
  fv <- Xp %*% coef(m2)
  fv
  
  d <- t(c(1, -1))
  # difference between linear predictor vals the points in two regions
  d %*% fv
  
  # variance of that estimate:
  d %*% Xp %*% m2$Vp %*% t(Xp) %*% t(d)
  
  
  
  

save.image('~/Desktop/R/GAM/GAM.RData')