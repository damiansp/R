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
data(chicago)
data(mack)
data(wesdr)


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
        

# 2.5 Comparing two surfaces
# Simulate a perturbed brain image from the model
brain1 <- brain
mu <- fitted(m2)
n <- length(mu)
ind <- brain1$X < 60 & brain1$Y < 20
mu[ind] <- mu[ind] / 3
#set.seed(1)
brain1$medFPQ <- rgamma(rep(1, n), mu / m1$sig2, scale=m2$sig2)
brain2 <- rbind(brain, brain1)
brain2$sample1 <- c(rep(1, n), rep(0, n))
brain2$sample0 <- 1 - brain2$sample1

m.same <- gam(medFPQ ~ s(Y, X, k=100), data=brain2, family=Gamma(link=log))
m.diff <- gam(medFPQ ~ s(Y, X, k=100) + s(Y, X, by=sample1, k=100), 
              data=brain2, 
              family=Gamma(link=log))
AIC(m.same, m.diff) # fitting different surfaces to each performs better
anova(m.diff)       # same


# 2.6 Prediction with predict.gam
predict(m2)[1:5]
pv <- predict(m2, se=T)
pv$fit[1:5]
pv$se[1:5] # on model scale
pv <- predict(m2, type='response', se=T)[1:5]
pv$fit[1:5]
pv$se[1:5] # on response scale

pd <- data.frame(X=c(80.1, 68.3), Y=c(41.8, 41.8))
predict(m2, newdata=pd)
predict(m2, newdata=pd, type='response', se=T)
predict(m3, newdata=pd, type='terms', se=T)

Xp <- predict(m2, newdata=pd, type='lpmatrix')
Xp
fv <- Xp %*% coef(m2)
fv

# Determine the difference of the linear predictor values at two points
d <- t(c(1, -1))
d %*% fv                                 # the diffrence
d %*% Xp %*% vcov(m2) %*% t(Xp) %*% t(d) # variance in the difference
(d %*% Xp) %*% vcov(m2) %*% (t(Xp) %*% t(d)) # same - avoids overly large matrices


# 2.7 Variance of non-linear functions of the fitted model
ind <- brain$region == 1 & !is.na(brain$region)
Xp <- predict(m2, newdata=brain[ind, ], type='lpmatrix')
# set.seed(11)
br <- rmvn(n=1000, coef(m2), vcov(m2)) # simulate from posterior
mean.FPQ <- rep(0,  1000)
for (i in 1:1000) {
  lp <- Xp %*% br[i, ] # replicate linear predictor
  mean.FPQ <- mean(exp(lp)) # replicate region 1 mean FPQ
}eti

# Same (more efficient, less readable)
mean.FPQ <- colMeans(exp(Xp %*% t(br)))
hist(mean.FPQ)



# 3. A Smooth ANOVA Model for Diabetic Retinopathy
head(wesdr)
dim(wesdr)
k <- 10
b <- gam(ret ~ s(dur, k=k) + s(gly, k=k) + s(bmi, k=k) + ti(dur, gly, k=k) 
           + ti(dur, bmi, k=k) + ti(gly, bmi, k=k), 
         select=T,
         data=wesdr,
         family=binomial(),
         method='ML')
summary(b)



# 4. Air Pollution in Chicago
head(chicago)
ap0 <- gam(death ~ s(time, bs='cr', k=200) + pm10median + so2median + o3median 
             + tmpd,
           data=chicago,
           family=poisson)
gam.check(ap0) # a few significant outliers
par(mfrow=c(2, 1))
plot(ap0, n=1000) # n increased to smooth
plot(ap0, residuals=T, n=1000)
chicago$death[3111:3125]

ap1 <- gam(death ~ s(time, bs='cr', k=200) + s(pm10median, bs='cr') 
             + s(so2median, bs='cr') + s(o3median, bs='cr') + s(tmpd, bs='cr'), 
           data=chicago, 
           family=poisson)
gam.check(ap1) # no apparent difference


# 4.1 A single index model for pollution related deaths
laggard <- function(x, n.lag=6) {
  n <- length(x)
  X <- matrix(NA, n, n.lag)
  for (i in 1:n.lag) { X[i:n, i] <- x[i:n - i + 1] }
  X
}
dat <- list(lag=matrix(0:5, nrow(chicago), 6, byrow=T))
dat$pm10 <- laggard(chicago$pm10median)

si <- function(theta, dat, opt=T) {
  # Return ML if opt==T else fitted GAM
  alpha <- c(1, theta) # unconstrained theta
  kk <- sqrt(sum(alpha^2))
  alpha <- alpha / kk  # constraint: ||alpha|| = 1
  o3 <- dat$o3 %*% alpha 
  tmp <- data$tmp %*% alpha
  pm10 <- dat$pm10 %*% alpha # reweight lagged covs
  b <- bam(dat$death ~ s(dat$time, k=200, bs='cr') + s(pm10, bs='cr') 
             + te(o3, tmp, k=8), 
           family=poisson)
  cat('.')
  if (opt) { return(b$gcv.ubre) }
  else {
    b$alpha <- alpha # add alpha to model object
    b$J <- outer(alpha, -theta / kk^2) # d alph_i / d thetaj
    for (j in 1:length(theta)) { b$J[j + 1, j] <- b$J[j + 1, j] + 1 / kk}
    b
  }
}

f1 <- optim(rep(1, 5), si, method='BFGS', hessian=T, dat=dat)  # BROKEN


# 4.2 A distributed lag model for pollution-related deaths
ap1 <- bam(death ~ s(time, bs='cr', k=200) + te(pm10, lag, k=c(10, 5)) 
             + te(o3, tmp, k=c(8, 8, 5)), 
           family=poisson, 
           data=dat)
           
    
           
# 5. Mackerel Egg Survey Example


# 5.1 Model development
head(mack)
mack$log.net.area <- log(mack$net.area)
gmtw <- gam(egg.count ~ s(lon, lat, k=100) + s(I(b.depth^0.5)) + s(c.dist) 
              + s(salinity) + s(temp.surf) + s(temp.20m) + offset(log.net.area), 
            data=mack, 
            family=tw, 
            method='REML',
            select=T)
gam.check(gmtw)
par(mfrow=c(2, 3))
plot(gmtw)

gm2 <- gam(egg.count ~ s(lon, lat, k=100) + s(I(b.depth^0.5)) + s(c.dist) 
             + s(temp.20m) + offset(log.net.area), 
           data=mack, 
           family=tw, 
           method='REML')
gam.check(gm2)
par(mfrow=c(2, 2))
plot(gm2)
summary(gm2)

plot(mack$temp.20m, resid(gm2))
lines(lowess(mack$temp.20m, resid(gm2)), col=2)
plot(mack$lat * mack$lon, resid(gm2))
lines(lowess(mack$lat * mack$lon, resid(gm2)), col=2)
plot(mack$temp.surf, resid(gm2))
lines(lowess(mack$temp.surf, resid(gm2)), col=2)
plot(mack$c.dist, resid(gm2))
lines(lowess(mack$c.dist, resid(gm2)), col=2)