###################################
# Ch. 6 Linear Statistical Models #
###################################

#6.1 An Analysis of Covariance Example
xyplot(Gas ~ Temp | Insul, whiteside, panel= function(x, y, ...){
	panel.xyplot(x, y, ...)
	panel.lmline(x, y, ...)
	}, xlab="Avg ext temp (°C)", ylab="Gas cons (1000 cft)", strip=function(...) strip.default(..., style=1))
	
gasB <- lm(Gas ~ Temp, data=whiteside, subset=Insul=="Before")
gasA <- update(gasB, subset=Insul=="After")
summary(gasB)
summary(gasA)
varB <- deviance(gasB) / gasB$df.resid	#direct calc; or:
varB <- summary(gasB)$sigma^2

gasBA <- lm(Gas ~ Insul / Temp -1, data=whiteside)
summary(gasBA)

gasQ <- lm(Gas~Insul / (Temp + I(Temp^2)) - 1, data=whiteside)
summary(gasQ)$coef

gasBA1 <- lm(Gas ~ Insul*Temp, data=whiteside)
summary(gasBA1)



#6.2 Model Formulae and Model Matrices
dat <- data.frame(a=factor(rep(1:3, 3)), y=rnorm(9, rep(2:4, 3), 0.1))
plot(dat$y ~ dat$a)
obj <- lm(y ~ a, dat); summary(obj)
(alf.star <- coef(obj))	#R: intercept is mean for level 1, a2 is amount by which level 2 differs from l1, and a3 is amt by which l3 differs from l1
Ca <- contrasts(dat$a)	#contrast matrix for 'a'
drop(Ca %*% alf.star[-1])	#Ca %*% c(coef(a2), coef(a3))
dummy.coef(obj)



#6.3 Regression Diagnostics
hills.lm <- lm(time ~ dist + climb, data=hills)
summary(hills.lm)
plot(fitted(hills.lm), studres(hills.lm))
abline(h=0, lty=2)
identify(fitted(hills.lm), studres(hills.lm), row.names(hills))
qqnorm(studres(hills.lm))
qqline(studres(hills.lm))
hills.hat <- lm.influence(hills.lm)$hat
cbind(hills, lev=hills.hat)[hills.hat > 3/35, ]
cbind(hills, pred=predict(hills.lm))["Knock Hill", ]
hills1.lm <- update(hills.lm, subset=-18); summary(hills1.lm)	#This point has small leveralge, so doesn't change model much
update(hills.lm, subset=-c(7,18))	#Pt 7 has both large resid and  leverage, and changes the model considerably
summary(update(hills1.lm, weights= 1/dist^2))	#expected precision should be proportional to the time (dist) of the race
#Force intercept to 0:
summary(lm(time ~ -1 + dist + climb, hills[-18,], weights=1/dist^2))
hills$ispeed <- hills$time / hills$dist
hills$grad <- hills$climb / hills$dist
hills2.lm <- lm(ispeed ~ grad, data=hills[-18,]); summary(hills2.lm)
plot(hills$grad[-18], studres(hills2.lm), xlab="grad")
abline(h=0, lty=2)
qqnorm(studres(hills2.lm))
qqline(studres(hills2.lm))



#6.4 Safe Prediction
quad1 <- lm(Weight ~ Days + I(Days^2), data=wtloss)
quad2 <- lm(Weight ~ poly(Days, 2), data=wtloss)	#orthogonal polynomials
new.x <- data.frame(Days=seq(250, 300,10), row.names=seq(250, 300, 10))
predict(quad1, newdata=new.x)
predict(quad2, newdata=new.x)



#6.5 Robust and Resistant Regression
phones.lm <- lm(calls ~ year, data=phones)
plot(phones$year, phones$calls)
abline(phones.lm)
abline(rlm(calls ~ year, phones, maxit=50), col=2)
abline(lqs(calls ~ year, phones), col=3)
legend(locator(1), lty=c(1,1,1), col=1:3, legend=c("least squares", "M-estimate", "LTS"))	#Indicator places upper left corner

summary(lm(calls~year, data=phones), cor=F)
summary(rlm(calls~year, maxit=50, data=phones), cor=F)
summary(rlm(calls~year, data=phones, psi=psi.bisquare), cor=F)

lqs(calls~year, data=phones)
