#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/GAM')

library(gamair)
#library(geoR)
library(MASS)
library(mgcv)
#library(rjags)
library(SemiPar)
library(survival)

data(bird)
data(brain)
data(cairo)
data(chicago)
data(coast)
data(gas)
data(mack)
data(mackp)
data(mpg)
data(pbc)
data(prostate)
data(sitka)
data(sole)
data(swer)
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


# 5.2 Model predictions
mackp$log.net.area <- 0
lon <- seq(-15, -1, 0.25)
lat <- seq(44, 58, 0.25)
zz <- array(NA, 57 * 57)
zz[mackp$area.index] <- predict(gm2, mackp)
image(lon, lat, matrix(zz, 57, 57), cex.lab=1.5, cex.axis=1.4)
contour(lon, lat, matrix(zz, 57, 57), add=T)
lines(coast$lon, coast$lat, col='lightgrey')

# set.seed(11)
br1 <- rmvn(n=1000, coef(gm2), vcov(gm2))
Xp <- predict(gm2, newdata=mackp, type='lpmatrix')
mean.eggs1 <- colMeans(exp(Xp %*% t(br1)))
hist(mean.eggs1)

br <- rmvn(n=1000, coef(gm2), vcov(gm2, unconditional=T))
Xp <- predict(gm2, newdata=mackp, type='lpmatrix')
mean.eggs <- colMeans(exp(Xp %*% t(br)))
hist(mean.eggs)


# 5.3 Alternative spatial smooths and geographical regression
gmgr <- gam(egg.count ~ s(lon, lat, k=100) + s(lon, lat, by=temp.20m) 
              + s(lon, lat, by=I(b.depth^0.5)) + offset(log.net.area), 
            data=mack, 
            family=tw, 
            method='REML')
gam.check(gmgr)
par(mfrow=c(1, 3))
plot(gmgr)



# 6. Spatial Smoothing of Portuguese Larks Data
head(bird)

bird$n <- bird$y / 1000
bird$e <- bird$x / 1000
m1 <- gam(crestlark ~ s(e, n, k=100), data=bird, family=binomial, method='REML')
summary(m1)

par(mfrow=c(1, 3))
plot(bird$x, bird$y, col=rgb(0, 0, 0, 0.2), pch=16)
points(bird$x, bird$y, col=bird$crestlark + 1, pch=16)
unique(bird$crestlark)
legend('topleft', pch=16, col=1:2, legend=c('absent', 'present'), bty='n')
plot(m1, scheme=2, contour.col=1, rug=F, too.far=0.03)
vis.gam(m1, plot.type='contour', too.far=0.03)

# Use Duchon spline (more flexible to boundary effects)
m2 <- gam(crestlark ~ s(e, n, bs='ds', m=c(1, 0.5), k=100), 
          data=bird, 
          family=binomial, 
          method='REML')
summary(m2)
plot(bird$x, bird$y, col=rgb(0, 0, 0, 0.2), pch=16)
points(bird$x, bird$y, col=bird$crestlark + 1, pch=16)
unique(bird$crestlark)
legend('topleft', pch=16, col=1:2, legend=c('absent', 'present'), bty='n')
plot(m2, scheme=2, contour.col=1, rug=F, too.far=0.03)
vis.gam(m2, plot.type='contour', too.far=0.03)

# Search for optimal Gaussian process smoother
REML <- r <- 1:10 * 10
for (i in 1:length(r)) {
  mt <- gam(crestlark ~ s(e, n, bs='gp', m=c(3, r[i]), k=100), 
            data=bird, 
            family=binomial, 
            method='REML')
  REML[i] <- mt$gcv.ubre
}
par(mfrow=c(1, 1))
plot(r, REML, type='l') # opt at rho (r) = 30
m3 <- gam(crestlark ~ s(e, n, bs='gp', m=c(3, 30), k=100), 
          data=bird, 
          family=binomial, 
          method='REML')
AIC(m1, m2, m3)

par(mfrow=c(1, 3))
plot(m1, scheme=2, contour.col=1, rug=F, too.far=0.03)
plot(m2, scheme=2, contour.col=1, rug=F, too.far=0.03)
plot(m3, scheme=2, contour.col=1, rug=F, too.far=0.03)

# Aggregate cells
bird$tet.n <- bird$N <- rep(1, nrow(bird))
bird$N[is.na(as.vector(bird$crestlark))] <- NA
ba <- aggregate(data.matrix(bird), by=list(bird$QUADRICULA), FUN=sum, na.rm=T)
ba$e <- ba$e / ba$tet.n
ba$n <- ba$n / ba$tet.n
m10 <- gam(cbind(crestlark, n - crestlark) ~ s(e, n, k=100), 
           data=ba, 
           family=binomial, 
           method='REML')
coords <- matrix(0, nrow(ba), 2)
coords[, 1] <- ba$e
coords[, 2] <- ba$n
gb <- list(data=resid(m10, type='d'), coords=coords)
par(mfrow=c(1, 1))
#plot(variog(gb, max.dist=100))
plot(fitted(m10), residuals(m10))



# 7. Generalized Additive Mixed Models in R


# 7.1 A space-time GAMM for sole eggs
solr <- sole
head(solr)
solr$station <- factor(with(solr, paste(-la, -lo, -t, sep='')))
som <- gamm(eggs ~ te(lo, la, t, bs=c('tp', 'tp'), k=c(25, 5), d=c(2, 1))
              + s(t, k=5, by=a.0),
            family=quasipoisson,
            data=solr,
            random=list(station=~1))
som$gam
som1 <- bam(eggs ~ te(lo, la, t, bs=c('tp', 'tp'), k=c(25, 5), d=c(2, 1))
              + s(t, k=5, by=a.0) + s(station, bs='re'),
            family=quasipoisson,
            data=solr)
gam.vcomp(som1)
som$lme

sole$station <- solr$station
# sw = 'soap wiggly'; bnd=???
som2 <- bam(
  eggs ~ te(lo, 
            la, 
            t, 
            bs=c('sw', 'cr'), 
            k=c(40, 5), 
            d=c(2, 1), 
            xt=list(list(bnd=bnd), NULL)) 
    + s(t, k=5, by=a.0) 
    + s(station, bs='re'), 
  knots=knots,
  family=quasipoisson,
  date=sole) 
  

# 7.2 The Temperature in Cairo
head(cairo)
plot(cairo$time, cairo$temp, type='l')
abline(lm(temp ~ time, data=cairo), col=2)
ctamm <- gamm(temp ~ s(day.of.year, bs='cc', k=20) + s(time, bs='cr'),
              data=cairo, 
              correlation=corAR1(form=~1|year))
summary(ctamm$gam)
intervals(ctamm$lme, which='var-cov')
ctamm$gam$sig2 / ctamm$gam$sp
par(mfrow=c(1, 2))
plot(ctamm$gam, scale=0)
par(mfrow=c(1, 1))

REML <- rho <- 0.6 + 0:20 / 100 # = seq(0.6, 0.8, 0.01)
for (i in 1:length(rho)) {
  ctbam <- bam(temp ~ s(day.of.year, bs='cc', k=20) + s(time, bs='cr'), 
               data=cairo, 
               rho=rho[i])
  REML[i] <- ctbam$gcv.ubre
}
cbind(REML, rho) # min(REMP) at rho = 0.69
(rho.opt <- rho[which(REML == min(REML))])

ctbam <- bam(temp ~ s(day.of.year, bs='cc', k=20) + s(time, bs='cr'), 
             data=cairo, 
             rho=rho.opt)
summary(ctbam)
par(mfrow=c(1, 2))
plot
par(mfrow=c(1, 1))


# 7.3 Full Bayesian Stochastic Simulations: jagam
jd <- jagam(log.size ~ s(days) + ozone, 
            data=sitka, 
            file='sitka0.jags', 
            diagonalize=T)
# Now edit the 'sitka0.jags' file to add more complicated random effects...
jd$jags.data$id <- sitka$id.num
jd$jags.data$nd <- length(unique(sitka$id.num))
# Following code depends on rjags 
#load.module('glm')
#jm <- jags.model('sitka.jags', data=jd$jags.data, inits=jd$jags.ini, n.chains=1)
#sam <- jags.samples(jm, c('b', 'rho', 'scale', 'mu'), n.iter=10000, thin=10)
#jam <- sim2jam(sam, jd$pregam)
#plot(jam)
#hist(sam$b[2, , 1]) # hist of ozone effect param
#pd <- data.frame(days=152:674, ozone=days * 0)
#Xp <- predict(jam, newdata=pd, type='lpmatrix')
#ii <- 1:25*20 + 500 # draws to select
#for (i in ii) {     # draw growth curves
#  fv <- Xp %*% sam$b[, i, 1] # growth curve from posterior
#  if (i == ii[1]) { plot(pd$days, fv, type='l') } else { lines(pd$days, fv) }
#}


# 7.4 Random Wiggly Curves
sitka$id.num <- as.factor(sitka$id.num)
b <- gamm(
  log.size ~ s(days) + ozone + ozone:days + s(days, id.num, bs='fs', k=5), 
  data=sitka)
plot(b$gam, pages=1)



# 8. Primary Biliary Cirrhosis Survival Analysis
head(pbc)
pbc$status1 <- as.numeric(pbc$status == 2)
pbc$stage <- factor(pbc$stage)
b0 <- gam(
  time ~ trt + sex + stage + s(sqrt(protime)) + s(platelet) + s(age) + s(bili) 
    + s(albumin) + s(sqrt(ast)) + s(alk.phos), 
  weights=status1, 
  family=cox.ph, 
  data=pbc)
anova(b0)
par(mfrow=c(2, 4))
plot(b0)
plot(b0$linear.predictors, residuals(b0))

np <- 300
newd <- data.frame(matrix(0, np, 0))
for (n in names(pbc)) { newd[[n]] <- rep(pbc[[n]][25], np) }
newd$time <- seq(0, 4500, length=np)

# predict and plot survival function
fv <- predict(b0, newdata=newd, type='response', se=T)
par(mfrow=c(1, 1))
plot(newd$time, fv$fit, type='l', ylim=c(0, 1), xlab='time', ylab='surv', lwd=2)
# crude 2se intervals
lines(newd$time, fv$fit + 2*fv$se.fit, col=4)
lines(newd$time, fv$fit - 2*fv$se.fit, col=4)
# cum hazard based intervals
se <- fv$se.fit / fv$fit
lines(newd$time, exp(log(fv$fit) + 2*se), col=2)
lines(newd$time, exp(log(fv$fit) - 2*se), col=2)


# 8.1 Time Dependent Covariates
# Taken from: ?cox.pht
app <- function(x, t, to) {
  ## wrapper to approx for calling from apply...
  y <- if (sum(!is.na(x))<1) rep(NA, length(to)) 
       else approx(t, x, to, method="constant", rule=2)$y
  if (is.factor(x)) factor(levels(x)[y], levels=levels(x)) 
  else y
} ## app

tdpois <- function(
    dat, event="z", et="futime", t="day", status="status1", id="id") {
  ## dat is data frame. id is patient id; et is event time; t is
  ## observation time; status is 1 for death 0 otherwise;
  ## event is name for Poisson response.
  if (event %in% names(dat)) warning("event name in use")
  require(utils) ## for progress bar
  te <- sort(unique(dat[[et]][dat[[status]]==1])) ## event times
  sid <- unique(dat[[id]])
  prg <- txtProgressBar(min=0, 
                        max=length(sid),
                        initial=0, 
                        char="=", 
                        width=NA, 
                        title="Progress", 
                        style=3)
  ## create dataframe for poisson model data
  dat[[event]] <- 0
  start <- 1
  dap <- dat[rep(1:length(sid), length(te)), ]
  for (i in 1:length(sid)) { ## work through patients
    di <- dat[dat[[id]]==sid[i], ] ## ith patient's data
    tr <- te[te <= di[[et]][1]] ## times required for this patient
    ## Now do the interpolation of covariates to event times...
    um <- data.frame(lapply(X=di, FUN=app, t=di[[t]], to=tr))
    ## Mark the actual event...
    if (um[[et]][1] == max(tr) && um[[status]] == 1) um[[event]][nrow(um)] <- 1 
    um[[et]] <- tr ## reset time to relevant event times
    dap[start:(start-1+nrow(um)), ] <- um ## copy to dap
    start <- start + nrow(um)
    setTxtProgressBar(prg, i)
  }
  close(prg)
  dap[1:(start - 1),]
} ## tdpois

# Convert pbcseq to equivalent Poisson form...
pbcseq$status1 <- as.numeric(pbcseq$status == 2) ## death indicator
pb <- tdpois(pbcseq) ## conversion
pb$tf <- factor(pb$futime) ## add factor for event time

# unlike lm, 'tf - 1' syntax ensures one coef estimated for each even time, not
# removal of intercept param
# Fit Poisson model...
b <- bam(z ~ tf - 1 + sex + trt + s(sqrt(protime)) + s(platelet)+ s(age)+
s(bili)+s(albumin), family=poisson,data=pb,discrete=TRUE,nthreads=2)

summary(b)

par(mfrow=c(2,3))
plot(b,scale=0, scheme=1)

# compute residuals...
chaz <- tapply(fitted(b),pb$id,sum) ## cum haz by subject
d <- tapply(pb$z,pb$id,sum) ## censoring indicator
mrsd <- d - chaz ## Martingale
drsd <- sign(mrsd)*sqrt(-2*(mrsd + d*log(chaz))) ## deviance

# plot survivor function and s.e. band for subject 25
te <- sort(unique(pb$futime)) ## event times
di <- pbcseq[pbcseq$id==25,] ## data for subject 25
pd <- data.frame(lapply(X=di,FUN=app,t=di$day,to=te)) ## interpolate to te
pd$tf <- factor(te)
X <- predict(b, newdata=pd, type='lpmatrix')
eta <- drop(X %*% coef(b))
H <- cumsum(exp(eta))
J <- apply(exp(eta) * X, 2, cumsum)
se <- diag(J %*% vcov(b) %*% t(J))^0.5
plot(stepfun(te, c(1, exp(-H))), 
     do.points=F, 
     ylim=c(0.7, 1), 
     ylab='S(t)', 
     xlab='t(days)', 
     main='', 
     lwd=2)
lines(stepfun(te, c(1, exp(-H + se))), do.points=F, col=2)
lines(stepfun(te, c(1, exp(-H - se))), do.points=F, col=2)
rug(pbcseq$day[pbcseq$id == 25])



# 9 Location-Scale Modeling
# Modeling assumptions: 
# acc[i] ~N(mu[i], var[i]); mu[i] = f1(time[i]); log(sd - b) - f2(time[i])
b <- gam(list(accel ~ s(times, bs='ad'), ~s(times, bs='ad')), 
         family=gaulss, 
         data=mcycle)
summary(b)
par(mfrow=c(1, 2))
plot(b) # acceleration, and log(shifted(sd))
# not clear, but I think params: mu and log(shifted(sd)) are givens in the 
# gaulss model-- check


# 9.1 Extreme Rainfall in Switzerland
b0 <- gam(
  list(
    exra ~ s(nao) + s(elevation) + climate.region 
      + te(N, E, year, d=c(2, 1), k=c(20, 5)), 
    ~ s(year) + s(nao) + s(elevation) + climate.region + s(N, E), 
    ~ s(elevation) + climate.region), 
  family=gevlss, 
  data=swer) 
  
# reducing...
b <- gam(list(exra ~ s(nao) + s(elevation) + climate.region + s(N, E), 
              ~ s(year) + s(elevation) + climate.region + s(N, E), 
              ~ climate.region), 
         family=gevlss, 
         data=swer) 
mu <- fitted(b)[, 1]
rho <- fitted(b)[, 2]
xi <- fitted(b)[, 3]
fv <- mu + exp(rho) * (gamma(1 - xi) - 1)/xi
par(mfrow=c(2, 3))
plot(b)

# GEV inverse CDF
Fi.gev <- function(z, mu, sigma, xi) {
  xi[abs(xi) < 1e-8] <- 1e-8 # appx 0 values by small xi values
  x <- mu + ((-log(z))^-xi - 1) * sigma/xi
}

mb <- coef(b) # posterior mean and...
Vb <- vcov(b) # cov
b1 <- b       # copy to modify
n.rep <- 1000
br <- rmvn(n.rep, mb, Vb) # posterior sim
n <- length(fitted(b))
sim.dat <- cbind(data.frame(rep(0, n * n.rep)), swer$code)
for (i in 1:n.rep) {
  b1$coefficients <- br[i, ] # copy sim coefs to gam obj
  X <- predict(b1, type='response')
  ii <- 1:n + (i - 1)*n
  sim.dat[ii, 1] <- Fi.gev(runif(n), X[, 1], exp(X[, 2]), X[, 3])
}

# now simulate mean and 98th percentile of annual max for ea station
stm <- tapply(sim.dat[, 1], sim.dat[, 2], mean)
st98 <- tapply(sim.dat[, 1], sim.dat[, 2], quantile, probs=0.98)



# 10. Fuel Efficiency of Cars: Multivariate Additive Models
b <- gam(
  list(city.mpg ~ fuel + style + drive + s(weight) + s(hp) + s(make, bs='re'),
       hw.mpg ~ fuel + style + drive + s(weight) + s(hp) + s(make, bs='re')),
  family=mvn(d=2),
  data=mpg)
par(mfrow=c(2, 3))
plot(b)
summary(b) 

# note make not signif for hw.mpg, and shape of both weight and hp similar in
# both mods.  Force identical smooths:
b1 <- gam(
  list(city.mpg ~ fuel + style + drive + s(hp) + s(weight) + s(make, bs='re'),
       hw.mpg ~ fuel + style + drive + s(make, bs='re'),
       1+2 ~ s(weight) + s(hp) - 1),
  family=mvn(d=2),
  data=mpg)
summary(b1)
plot(b1)



# 11. Functional Data Analysis


# 11.1 Scalar on function regression
# mod: octane[i] = integral(f(v)k[i](v)dv) + e[i]
# k[i](v) is the spectrum as a function of frequency, v
b <- gam(octane ~ s(nm, by=NIR, k=50), data=gas)
plot(b, scheme=1, col=4)
plot(fitted(b), gas$octane)
abline(lm(fitted(b) ~ gas$octane))

b <- gam(type ~ s(MZ, by=intensity, k=100), 
         family=ocat(R=3), 
         data=prostate, 
         method='ML')
plot(b, scheme=1, xlab='Daltons', ylab='f(D)', cex.lab=1.6, cex.axis=1.4)
pb <- predict(b, type='response') # matrix of class probs
plot(factor(prostate$type), pb[, 3])
qq.gam(b, rep=100, lev=0.95)