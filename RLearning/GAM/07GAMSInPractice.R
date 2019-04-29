#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/GAM')

library(gamair)
library(MASS)
library(mgcv)

data(brain)


# 1. Specifying Smooths


# 1.1 How smooth specification works
# Set up a smoother
head(mcycle)
sm <- smoothCon(s(times, k=10), data=mcycle, knots=NULL)[[1]]
# Use to fit a regression spline model
beta <- coef(lm(mcycle$accel ~ sm$X - 1))
# plot
with(mcycle, plot(times, accel))
# prediction times
times <- seq(0, 60, length=200)
# Get matrix mapping beta to spline prediction at <times>
Xp <- PredictMat(sm, data.frame(times=times))
lines(times, Xp %*% beta, col=2)



# 2. Brain Image Example


# 2.1 Preliminary modeling
head(brain)
brain <- brain[brain$medFPQ > 5e-3, ] # exclude 2 outliers

# Skew and fact that all values are positive in response suggest tranformation 
# neccessary if using a Guassian model:
m0 <- gam(medFPQ ~ s(Y, X, k=100), data=brain)
gam.check(m0)

# Clear heteroskedasticity... assume variance roughly proportional to µ^beta:
e <- residuals(m0)
fv <- fitted(m0)
lm(log(e^2) ~ log(fv)) 
# i.e., log(fv) = beta = 1.912 (var increases with the square of the mean)

hist(brain$medFPQ^1.912)
hist(brain$medFPQ^0.25)
m1 <- gam(medFPQ^0.25 ~ s(Y, X, k=100), data=brain)
gam.check(m1)

m2 <- gam(medFPQ ~ s(Y, X, k=100), data=brain, family=Gamma(link=log))
gam.check(m2)

mean(fitted(m1)^4) # 0.9856
mean(fitted(m2))   # 1.2115
mean(brain$medFPQ) # 1.2503

m2
vis.gam(m2, plot.type='contour', n.grid=60, too.far=0.03)


# 2.2 Would an additive structure be better?
# (noting that m2 uses 61 df, to fit a rather complicated surface)
m3 <- gam(medFPQ ~ s(Y, k=30) + s(X, k=30), data=brain, family=Gamma(link=log))
m3 # note only 31 df, but GCV is somewhat worse
AIC(m2, m3) # m2 also has a better AIC
# pattern also unrealistic:
vis.gam(m3, plot.type='contour', n.grid=60, too.far=0.03) 


# 2.3 Isotropic or tensor product smooths?
tm <- gam(medFPQ ~ te(Y, X, k=10), data=brain, family=Gamma(link=log))
tm1 <- gam(medFPQ ~ s(Y, k=10, bs='cr') + s(X, k=10, bs='cr') + ti(X, Y, k=10),
           data=brain,
           family=Gamma(link=log))
AIC(m2, tm, tm1)
anova(tm1)


# 2.4 Detecting symmetry (with 'by' variables)
head(brain)
mu <- mean(brain$X)
brain$Xc <- abs(brain$X - mu)
brain$right <- as.numeric(brain$X < mu)
m.sym <- gam(medFPQ ~ s(Y, Xc, k=100), data=brain, family=Gamma(link=log))
m.asym <- gam(medFPQ ~ s(Y, Xc, k=100) + s(Y, Xc, k=100, by=right), 
              data=brain, 
              family=Gamma(link=log))
m.sym
m.asym
anova(m.asym) # the :right  term would not be signif if symmetric

vis.gam(m.sym,
        plot.type='contour', 
        view=c('Xc', 'Y'), 
        too.far=0.03, 
        n.grid=60,
        zlim=c(-1, 2),
        main='both sides')
vis.gam(m.asym,
        plot.type='contour', 
        view=c('Xc', 'Y'), 
        cond=list(right=0),
        too.far=0.03, 
        n.grid=60,
        zlim=c(-1, 2),
        main='left')
vis.gam(m.asym,
        plot.type='contour', 
        view=c('Xc', 'Y'), 
        cond=list(right=1),
        too.far=0.03, 
        n.grid=60,
        zlim=c(-1, 2),
        main='right')