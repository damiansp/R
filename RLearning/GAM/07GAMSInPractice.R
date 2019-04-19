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