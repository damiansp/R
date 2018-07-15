#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')
library(numDeriv)


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


                         
