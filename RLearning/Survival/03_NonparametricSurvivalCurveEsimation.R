#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')

library(asaur)
library(muhaz)    # est./plot nonparam hazard funcs
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
summary(result.km)
plot(result.km, 
     col=c(1, 2, 2), 
     lty=c(1, 1, 1), 
     mark='•', 
     xlab='Time (months)', 
     ylab='Survival Probability')


# 2. Finding the Median Survival and a Confidence Interval for the Median
result.km
abline(h=0.5, col='grey')
abline(v=10.3, col=4) # median survival
abline(v=c(5.79, 15.27), col=4, lty=4)



# 3. Median Follow-Up Time
delta.followup <- 1 - delta
survfit(Surv(time.months, delta.followup) ~ 1)
# potential follow up time (i.e., followup time, had they not died) has median of 
# 27.8 months, whereas the simple median:
median(time.months)
# ...is only 9.95 months 



# 4. Obtaining a Smoothed Hazard and Survival Function Estimate
t.vec <- c(7, 6, 6, 5, 2, 4)
cens.vec <- c(0, 1, 0, 0, 1, 1)
result.simple <- muhaz(t.vec, 
                       cens.vec, 
                       max.time=8, 
                       bw.grid=2.25,       # smoothing param
                       bw.method='global', 
                       b.cor='none')       # boundary correction
plot(result.simple)
result.pe5 <- pehaz(time.months, delta, width=5, max.time=20)
plot(result.pe5, ylim=c(0, 0.13))
result.pe1 <- pehaz(time.months, delta, width=1, max.time=20)
lines(result.pe1, col='grey', lty=1)
result.smooth <- muhaz(time.months, 
                       delta, bw.smooth=20, 
                       b.cor='left', 
                       bw.method='global', 
                       max.time=20)
lines(result.smooth, col=2, lty=4)
result.smooth <- muhaz(
  time.months, delta, bw.smooth=20, b.cor='left', max.time=20)
lines(result.smooth, col=2)

# Obtain smooth est of surv function from hazard
haz <- result.smooth$haz.est
times <- result.smooth$est.grid
surv <- exp(-cumsum(haz[1:(length(haz) - 1)] * diff(times)))
result.km <- survfit(Surv(time.months, delta) ~ 1, conf.type='none')
plot(result.km,
     conf.int=F, 
     mark='*', 
     xlab='Time (months)', 
     ylab='Survival Prob.',
     xlim=c(0, 30))
lines(surv ~ times[1:length(times) - 1], col=2)