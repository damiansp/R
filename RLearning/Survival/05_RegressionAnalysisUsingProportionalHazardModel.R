#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')


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

