#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/GAM')

library(MASS)
library(mgcv)



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