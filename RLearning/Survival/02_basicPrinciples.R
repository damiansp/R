#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')


library(survival)

# 1. Hazard and Survival Functions
head(survexp.us)
str(survexp.us)
dimnames(survexp.us)
tm <- c(0,      # birth
        1/365,  # 1 day
        7/365,  # 1 week
        28/365, # 4 weeks
        1:109)  # subsequent years
tm <- 0:109
haz.male <- survexp.us[, 'male', '2004']     # males 2004 cohort
haz.female <- survexp.us[, 'female', '2004'] # females 2004 cohort

plot(haz.male ~ tm, 
     type='l', 
     log='y', 
     xlab='Age (years)', 
     ylab='log(Hazard)',
     main='Expected Death Hazard for Americans Born in 2004')
lines(haz.female ~ tm, col=2)
legend('bottomright', lty=1, col=1:2, legend=c('male', 'female'), bty='n')



# 4. Parametric Survival Distributions
# Weibull survival distribution, where shape = alpha, and scale = 1/lambda
weib.surv <- function(t, shape, scale) {
  pweibull(t, shape=shape, scale=scale, lower.tail=F)
}

alpha = 1.5
lambda = 0.03

par(mfrow=c(2, 1))
curve(weib.surv(x, shape=alpha, scale=1 / lambda), 
      from=0, 
      to=80, 
      ylim=c(0, 1), 
      ylab='Survival Probability', 
      xlab='Time')
      
weib.haz <- function(x, shape, scale) {
  dweibull(x, shape=shape, scale=scale) / weib.surv(x, shape, scale)
}

curve(weib.haz(x, shape=alpha, scale=1 / lambda), 
      from=0, 
      to=80, 
      ylab='Hazard', 
      xlab='Time')

# 1000 rand vars
n <- 1000
tt.weib <- rweibull(n, shape=alpha, scale=1 / lambda)
mean(tt.weib) # ~
gamma(1 + 1/alpha) / lambda
median(tt.weib) # ~
(log(2))^(1/alpha) / lambda


# Gamma distribution
gamma.haz <- function(x, shape, scale) {
  dgamma(x, shape=shape, scale=scale) / 
    pgamma(x, shape=shape, scale=scale, lower.tail=F)
}

par(mfrow=c(1, 1))
curve(gamma.haz(x, alpha, 1 / lambda), 
      from=0, 
      to=80, 
      ylab='Hazard', 
      xlab='Time',
      ylim=c(0, 0.065),
      col=2)
curve(gamma.haz(x, shape=1, 1 / lambda), add=T)
curve(gamma.haz(x, shape=0.75, 1 / lambda), col=4, add=T)



# 5. Computing the Survival Function from the Hazard Function
tm.diff <- diff(tm)
surv.male <- exp(-cumsum(haz.male * tm.diff) * 365.25)
surv.female <- exp(-cumsum(haz.female * tm.diff) * 365.25)
plot(surv.male, type='l')
lines(surv.female, col=2)
abline(h=0.5, col='grey', lty=4)
sum(surv.male * tm.diff)   # mean male survival (2004)
sum(surv.female * tm.diff) # female



# 6. A Brief Introduction to Maximum Likelihood Estimation

