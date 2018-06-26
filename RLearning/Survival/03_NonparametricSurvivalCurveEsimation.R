#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')

library(asaur)
library(survival)

# 1. Nonparametric Estimation of Survival Function
tt <- c(7, 6, 6, 5, 2, 4)   # survival times
cens <- c(0, 1, 0, 0, 1, 1) # censured?
Surv(tt, cens)

# Kaplan-Meier method
result.km <- survfit(Surv(tt, cens) ~ 1, conf.type='log-log')
result.km
# n = subjects; events = deaths/failures; median = median survival 
# + 95%CI for median
summary(result.km)
plot(result.km, col=c(1, 2, 2))

# Nelson-Aalen estimate
result.fh <- survfit(Surv(tt, cens) ~ 1, conf.type='log-log', type='fh')
result.fh # same
summary(result.fh)
plot(result.fh, col=c(1, 2, 2))
lines(result.km, col=c(4, 5, 5))

time.months <- gastricXelox$timeWeeks * 7 / 30.25
delta <- gastricXelox$delta
result.km <- survfit(Surv(time.months, delta) ~ 1, conf.type='log-log')
result.km
summary(result.km)
plot(result.km, 
     col=c(1, 2, 2), 
     lty=c(1, 1, 1), 
     mark='•', 
     xlab='Time (months)', 
     ylab='Survival Probability')



# 2. Finding the Median Survival and a Confidence Interval for the Median
