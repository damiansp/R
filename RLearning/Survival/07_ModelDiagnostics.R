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


# 1.2 Case Deletion (AKA Jackknife) Residuals
res <- coxph(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ 
               pharmacoSmoking$grp + pharmacoSmoking$employment 
               + pharmacoSmoking$age)
coef.all <- res$coef[4] # age
coef.all

n <- length(pharmacoSmoking$ttr)
jk.beta.vec <- rep(NA, n) # jackknife
for (i in 1:n){
  tt.i <- pharmacoSmoking$ttr[-i]
  delta.i <- pharmacoSmoking$relapse[-i]
  grp.i <- pharmacoSmoking$grp[-i]
  employment.i <- pharmacoSmoking$employment[-i]
  age.i <- pharmacoSmoking$age[-i]
  res.i <- coxph(Surv(tt.i, delta.i) ~ grp.i + employment.i + age.i)
  coef.i <- res.i$coef[4]
  jk.beta.vec[i] <- (coef.all - coef.i)
}

index.obs <- 1:n
plot(jk.beta.vec ~ index.obs, 
     type='h', 
     xlab='Observation', 
     ylab='Change in Coef for Age')
abline(h=0)
identify(jk.beta.vec ~ index.obs) # 46, 68

resid.df.beta <- residuals(res, type='dfbeta')
index.obs <- 1:n
plot(resid.df.beta[, 4] ~ index.obs, type='h', xlab='Obs', ylab='Delta Coef.')
abline(h=0)



# 2 Checking the Proportions Hazards Assumption
# 2.1 Log cumulative hazard plots
head(pancreatic)
date.format <- '%m/%d/%Y'
pancreatic$progression <- as.Date(as.character(pancreatic$progression), 
                                  format=date.format)
pancreatic$onstudy <- as.Date(as.character(pancreatic$onstudy), 
                              format=date.format)
pancreatic$death <- as.Date(as.character(pancreatic$death), format=date.format)
progression.only <- pancreatic$progression - pancreatic$onstudy
overall.survival <- pancreatic$death - pancreatic$onstudy
pfs <- progression.only
pfs[is.na(pfs)] <- overall.survival[is.na(pfs)]
pfs.month <- pfs / 30.417

res.surv.LA <- survfit(Surv(pfs.month) ~ pancreatic$stage, 
                       subset=(pancreatic$stage == 'LA'))
time.LA <- res.surv.LA$time
surv.LA <- res.surv.LA$surv
c.log.log.LA <- log(-log(surv.LA))
logtime.LA <- log(time.LA)
res.surv.M <- survfit(Surv(pfs.month) ~ pancreatic$stage, 
                      subset=(pancreatic$stage == 'M'))
time.M <- res.surv.M$time
surv.M <- res.surv.M$surv
c.log.log.M <- log(-log(surv.M))
logtime.M <- log(time.M)
plot(c.log.log.LA ~ logtime.LA, type='s', col=4, ylim=c(-2, 1.5))
lines(c.log.log.M ~ logtime.M, col=2, type='s')
legend(
  'bottomright', legend=c('Locally Advanced', 'Metastatic'), col=c(4, 2), lty=1)
  

# 2.2 Schoenfield Residuals