#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')
library(numDeriv)
library(survival)


# 2. Comparing Two Survival Distributions Using a Partial Likelihood Function
partial.likelihood.simple <- function(beta) {
  psi <- exp(beta)
  log(psi) - log(3*psi + 3) - log(3*psi + 1) - log(2*psi + 1)
}

result <- optim(par=0, 
                fn=partial.likelihood.simple, 
                method='L-BFGS-B', 
                control=list(fnscale=-1), 
                lower=-3, 
                upper=1)
result # value is the partial likelihood, par is the parameter that maximizes it

betas <- seq(-4, 1, length=100)
p.likelihoods <- partial.likelihood.simple(betas)
plot(p.likelihoods ~ betas, type='l')
abline(h=result$value, col=2)
abline(v=result$par, col=4, lty=2)



# 3. Partial Likelihood Hypothesis Tests
# 3.3 The likelihood ratio test
head(pharmacoSmoking)
#?coxph # proportional hazards
result.cox <- coxph(Surv(tt, status) ~ grp) # data???
summary(result.cox) # will give scores for Likelihood ratio test, Wald test and 
                    # Score (logrank) test
grad(func=partial.likelihood.simple, x=0) # slope of tangent at 1 SE, and hence 
                                          # measure of information
hessian(func=partial.likelihood.simple, x=0) # 2nd derivative, or curvature at 1 SE
pchisq(1.274, df=1, lower.tail=F) 
h <- hessian(func=partial.likelihood.simple, x=result.cox$par)
sqrt(1 / -h) # SE
p <- 2 * pnorm(coef / sqrt(1 / -h), lower.tail=F)
beta.hat <- result.cox$par
likelihood.ratio <- 2 * (partial.likelihood.simple(beta.hat) 
                         - partial.likelihood.simple(0))
p <- pchisq(likelihood.ratio, 1, lower.tail=F)



# 6. Handling of Tied Survival Times
tt     <- c(7, 6, 6, 5, 2, 4, 4, 1, 3, 1)
status <- c(0, 1, 0, 0, 1, 1, 1, 1, 0, 1)
grp    <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)

loglik.continuous <- function(b) {
  (3*b 
   + log(exp(b) + 9) 
   - log(4*exp(b) + 6) 
   - log(3*exp(b) + 6) 
   - log(2*exp(b) + 6) 
   - log(exp(b) + 5) 
   - log(exp(b) + 4))
}

loglik.discrete <- function(b) {
  result.a <- exp(2*b) / (6*exp(2*b) + 24*exp(b) + 15)
  result.b <- 1 / (6 + 2*exp(2*b))
  result.c <- exp(b) / (10 + 5*exp(b))
  log(result.a) + log(result.b) + log(result.c)
}

result.optim.continuous <- optim(
  par=1.4, fn=loglik.continuous, method='BFGS', control=list(fnscale=-1))                         
result.optim.discrete <- optim(
  par=1.4, fn=loglik.discrete, method='BFGS', control=list(fnsacle=-1))
result.optim.continuous
result.optim.discrete
result.coxph <- coxph(Surv(tt, status) ~ grp, ties='exact') 
result.coxph$coef



# 7. Left Truncation
# time from entry into trial until death or censoring
tt <- c(6, 7, 10, 15, 19, 25) 
status <- c(1, 0, 1, 1, 0, 1) # death observed
grp <- c(0, 0, 1, 0, 1, 1)    # experimental group/treatment
# time before entry that patient was diagnosed
back.time <- c(-3, -11, -3, -7, -10, -5) 
coxph(Surv(tt, status) ~ grp)

tm.enter <- -back.time
tm.exit <- tt - back.time
coxph(Surv(tm.enter, tm.exit, status, type='counting') ~ grp)
