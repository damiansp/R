#===============================================#
#                                               #
#  Nonparametric Statistical Methods Using R    #
#    Kloke & McKean (2015)                      #
#                                               #
#                      #========================#
#                      #
#  Ch 4: Regression I  #
#                      #
#======================#

rm(list = ls())
setwd('~/Learning/R/RLearning/NonparametricStats')
library(boot)
library(npsm)
library(quantreg)
library(Rfit)
library(sm)

data(engel)
data(speed)
#load('~/Desktop/R/NonparametricStats/NPS.RData')



# 2 Simple Linear Regression
plot(engel)
r.fit <- rfit(foodexp ~ income, dat=engel)
summary(r.fit)
lm.fit <- lm(foodexp ~ income, dat=engel)
summary(lm.fit)

abline(r.fit, col=2)
abline(lm.fit, col=4)
legend('topleft', 
       lty=1, 
       col=c(2, 4), 
       legend=c('Rank-based', 'Least squares'), 
       title='Model' )

par(mfrow=c(2, 2))
plot(lm.fit)

rs <- rstudent(r.fit)	# standardized resids
y.hat <- fitted.values(r.fit)
par(mfrow = c(1, 2))
qqnorm(rs)
qqline(rs, col = 'grey')
plot(y.hat, rs)	# equiv. to the second and first plots in plot(lm)
lines(lowess(rs ~ y.hat), col = 2)
par(mfrow=c(1, 1))




# 3 Multiple Linear Regression
# 3.1 Multiple Regression
r.fit <- rfit(ffa ~ age + weight + skin, data=ffa)
summary(r.fit)
	
r.fit.red <- rfit(ffa ~ weight + skin, data=ffa)
summary(r.fit.red)
drop.test(r.fit, r.fit.red) # the large p val indicates a non-signif drop in 
                            # dispersion between the models, favoring the 
                            # smaller model                            
	
# 3.2 Polynomial Regression
fit <- rfit(sp ~ mpg + I(mpg^2), data=speed)
summary(fit)
lm.fit <- lm(sp ~ mpg + I(mpg^2), data=speed)
summary(lm.fit)
plot(sp ~ mpg, data = speed)
xv <- seq(min(speed$mpg), max(speed$mpg), length=100)
yv.r <- coef(fit)[1] + coef(fit)[2]*xv + coef(fit)[3]*xv^2
yv.ls <- coef(lm.fit)[1] + coef(lm.fit)[2]*xv + coef(lm.fit)[3]*xv^2
lines(xv, yv.r, col=2)
lines(xv, yv.ls, col=4)
legend('topright', 
       lty=1, 
       col=c(2, 4), 
	   legend=c('Rank-based', 'Least Squares'), 
	   title='Model')

plot(resid(fit) ~ fitted(fit))
par(mfrow=c(2, 2))
plot(lm.fit)
par(mfrow=c(1, 1))
	


# 5 Aligned Rank Tests
k <- 3	# n treatments
p <- 2	# n covariates
n <- 10 # n subjects per treatment
N <- n * k	# n total subjects
y <- rnorm(N)
x <- matrix(rnorm(N * p), ncol=p)
g <- rep(1:k, each=n)
aligned.test(x, y, g)



# 6 Bootstrap
boot.rfit <- function(data, indices) {
  data <- data[indices, ]
  fit <- rfit(weight ~ height, data=data, tau='N')
  coef(fit)[2]	# return coef for height only
}

bb.boot <- boot(data=baseball, statistic=boot.rfit, R=1000)
bb.boot
plot(bb.boot)
boot.ci(bb.boot, type='perc', index=1)



# 7. Nonparametric Regression
# 7.1 Polynomial Models
data(poly)
head(poly)
plot(y ~ x, data=poly)
deg <- polydeg(poly[,'y'], poly[,'x'], 5, alpha=0.05)
deg
summary(deg$fitf)
xv <- seq(-6, 10, 0.1)
yv <- (coef(deg$fitf)[1] 
       + coef(deg$fitf)[2]*xv 
       + coef(deg$fitf)[3]*xv^2 
       + coef(deg$fitf)[4]*xv^3)
lines(yv ~ xv, col=4)
	
# 7.2 Nonparametric Regression
par(mfrow=c(2, 2))
plot(y ~ x, data=poly, col='lightgrey')
title('Bandwidth: 2.0')
lines(ksmooth(poly[, 'x'], poly[, 'y'], kernel='normal', bandwidth=2), col=2)
plot(y ~ x, data=poly, col='lightgrey')
title('Bandwidth: 1.4')
lines(ksmooth(poly[, 'x'], poly[, 'y'], kernel='normal', bandwidth=1.4 ), col=2)
plot(y ~ x, data=poly, col='lightgrey')
title('Bandwidth: 0.7')
lines(ksmooth(poly[, 'x'], poly[, 'y'], kernel='normal', bandwidth = 0.7), col=2)
plot(y ~ x, data=poly, col='lightgrey')
title('Bandwidth: 0.1')
lines(ksmooth(poly[, 'x'], poly[, 'y'], kernel='normal', bandwidth=0.1), col=2)
	
	
# Ex. 7.2 Sine-Cosine Model
data(sincos)
head(sincos)
par(mfrow=c(1, 1))
plot(sincos)
fit <- sm.regression(sincos$x, sincos$y, display='none')
fit$h  # data-driven bandwidth
plot(y ~ x, data=sincos)
with(fit, lines(estimate ~ eval.points, col=2))

# sm.regression smoother is not robust consider adding an outlier
sincos2 <- sincos
sincos2 <- rbind(sincos2, data.frame(x=38, y=800))
fit2 <- sm.regression(sincos2$x, sincos2$y, display='none')
#plot(y ~ x, data=sincos2)
with(fit2, lines(estimate ~ eval.points, col=4))
fit3 <- loess(sincos2$y ~ sincos2$x, family='symmetric', span=0.35) # robust fit
summary(fit3)
lines(sincos2$x[-198], fit3$fitted[-198], col=5)
fit4 <- loess(sincos2$y ~ sincos2$x, span=0.35) # Least Squares fit
summary(fit4)
lines(sincos2$x[-198], fit4$fitted[-198], col=6)

data(faithful)
head(faithful)
plot(faithful)
sm.mod <- sm.regression(faithful$eruptions, faithful$waiting, display='none')
with(sm.mod, lines(estimate ~ eval.points, col=2))
loess.mod <- loess(waiting ~ eruptions, data=faithful)
lines(faithful$eruptions[order(faithful$eruptions)], 
      loess.mod$fitted[order(faithful$eruptions)], 
      col=4)
loess.rob.mod <- loess(waiting ~ eruptions, data=faithful, family='symmetric')
lines(faithful$eruptions[order(faithful$eruptions)], 
      loess.rob.mod$fitted[order(faithful$eruptions)], 
      col=5)

	
# 8 Correlation
# 8.4 Computation and Examples
# Example 8.1 Baseball Data, 2014
# Data from: http://baseballguru.com/bbdata1.html
bb <- read.csv('mlb2014.csv')
names(bb)
	
plot(bb$HR ~ bb$avg, pch = 16, col = rgb(0, 0, 0, 0.25))
abline(lm(HR ~ avg, data=bb), col=2)
cor.test(bb$avg, bb$HR)                    # 0.315
cor.test(bb$avg, bb$HR, method='spearman') # 0.374
cor.test(bb$avg, bb$HR, method='kendall')  # 0.271
	
args(cor.boot.ci)
	
	
	# Example 4.8.2 (previous cont'd)
	cor.boot.ci(bb$avg[!is.na(bb$avg)], bb$HR[!is.na(bb$avg)])
	
	cor.boot.ci(bb$avg[!is.na(bb$avg)], bb$HR[!is.na(bb$avg)], method='pearson')
	
	cor.boot.ci(bb$avg[!is.na(bb$avg)], bb$HR[!is.na(bb$avg)], method='kendall')
	
	
	
	
	



save.image('~/Desktop/R/NonparametricStats/NPS.RData')