#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')

library(asaur)
library(survival)

# 1. Assessing Goodness of Fit Using Residuals
# 1.1 Martingale and deviance residuals

head(pharmacoSmoking)
pharmacoSmoking$priorAttemptsT <- pharmacoSmoking$priorAttempts
pharmacoSmoking$priorAttemptsT[pharmacoSmoking$priorAttempts > 20] <- 20

res0 <- coxph(Surv(ttr, relapse) ~ 1, pharmacoSmoking)
rr0 <- residuals(res0, type='martingale')

par(mfrow=c(2, 3))
scatter.smooth(rr0 ~ pharmacoSmoking$age)
scatter.smooth(rr0 ~ log(pharmacoSmoking$age))
scatter.smooth(rr0 ~ pharmacoSmoking$priorAttemptsT)
scatter.smooth(rr0 ~ log(pharmacoSmoking$priorAttemptsT + 1))
scatter.smooth(rr0 ~ pharmacoSmoking$longestNoSmoke)
scatter.smooth(rr0 ~ log(pharmacoSmoking$longestNoSmoke + 1))

res.grp <- coxph(Surv(ttr, relapse) ~ grp, pharmacoSmoking)
res.step <- step(
  res.grp, 
  scope=list(upper=~grp + gender + race + employment + yearsSmoking 
               + levelSmoking + age + priorAttemptsT + log(longestNoSmoke + 1),
             lower=~grp))
res.step
rr.final <- residuals(res.step, type='martingale')
par(mfrow=c(2, 2))
scatter.smooth(rr.final ~ pharmacoSmoking$age)
plot(rr.final ~ pharmacoSmoking$grp)
plot(rr.final ~ pharmacoSmoking$employment)