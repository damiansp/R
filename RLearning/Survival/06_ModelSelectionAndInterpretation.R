#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')

library(survival)


# 1. Covariate Adjustment
lambda.mutant.0 <- 0.03 # hazard
lambda.mutant.1 <- 0.03 * 0.55
lambda.wt.0 <- 0.03 * 0.2
lambda.wt.1 <- 0.03 * 0.2 * 0.55
tt.control.mutant <- rexp(25, rate=lambda.mutant.0)
tt.treat.mutant <- rexp(125, lambda.mutant.1)
tt.control.wt <- rexp(125, lambda.wt.0)
tt.treat.wt <- rexp(25, lambda.wt.1)
tt.all <- c(tt.control.mutant, tt.treat.mutant, tt.control.wt, tt.treat.wt)
status <- rep(1, length(tt.all))
genotype <- c(rep('mutant', 150), rep('wt', 150))
trt <- c(rep(0, 25), rep(1, 125), rep(0, 125), rep(1, 25))

coxph(Surv(tt.all, status) ~ trt)
trt.coef <- 0.422
exp(trt.coef) # 1.53, incorrectly suggesting 53% *additional* risk for experimental 
              # group over control
coxph(Surv(tt.all, status) ~ trt + strata(genotype))
exp(-0.349)   # Now suggests risk is only 70% of control

# Can explicitly est. genetic effect:
coxph(Surv(tt.all, status) ~ trt + genotype)
# trt reduces hazard, but wild type (wt) genetic variant also has lower hazard



# 2. Categorical and Continuous Covariates
race <- factor(c('b', 'b', 'w', 'w', 'o', 'o'))
age <- c(48, 52, 87, 82, 67, 53)
model.matrix(~ race + age)

race <- relevel(race, ref='w')
model.matrix(~ race + age)
model.matrix(~ race * age)

age <- runif(600, 40, 80)
race <- factor(c(rep('w', 200), rep('b', 200), rep('o', 200)))
race <- relevel(race, ref='w')

log.rate.vec <- -4.5 * c(rep(0, 200), rep(1, 200), rep(2, 200)) + age*0.05
tt <- rexp(n=600, rate=exp(log.rate.vec))
status <- rep(1, 600)

result.cox <- coxph(Surv(tt, status) ~ race + age)
summary(result.cox)

# coefs similar to sim inputs (1, 2 * -4.5 for race 0.05 for age vs 0.05)



# 3. Hypothesis Testing for Nested Models
head(pharmacoSmoking)
mod.a <- coxph(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ 
                 pharmacoSmoking$ageGroup4)
mod.a
mod.b <- coxph(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ 
                 pharmacoSmoking$employment)
mod.b
mod.c <- coxph(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ 
                 pharmacoSmoking$ageGroup4 + pharmacoSmoking$employment)
mod.c
logLik(mod.a)
logLik(mod.b)
logLik(mod.c)
(ratio.stat.ac <- 2 * (logLik(mod.c) - logLik(mod.a)))
(ratio.stat.bc <- 2 * (logLik(mod.c) - logLik(mod.b)))
pchisq(as.numeric(ratio.stat.ac), df=2, lower.tail=F)
pchisq(as.numeric(ratio.stat.bc), df=3, lower.tail=F)

mod.0 <- coxph(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ 1)
mod.0
logLik.0 <- -386.153 # mod output
(ratio.stat.c0 <- 2 * (logLik(mod.c) - logLik.0))

anova(mod.a, mod.c)



# 4. AIC for Comparing Non-Nested Models
AIC(mod.a)
AIC(mod.b)
AIC(mod.c)

mod.all <- coxph(
  Surv(ttr, relapse) ~ grp + gender + race + employment + yearsSmoking 
    + levelSmoking + ageGroup4 + priorAttempts + longestNoSmoke, 
  data=pharmacoSmoking)
result.step <- step(
  mod.all, 
  scope=list(upper=~grp + gender+ race + employment + yearsSmoking 
               + levelSmoking + ageGroup4 + priorAttempts + longestNoSmoke,
             lower=~grp))
result.step             