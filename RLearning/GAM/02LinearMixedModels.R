#=========#=========#=========#=========#=========#=========#=========#=========
rm(list = ls())
setwd('~/Learning/R/RLearning/GAM')
library(gamair)
library(lme4)
library(nlme)
data(Machines)
data(Rail)
data(stomata)



# 1. Mixed Models for Balanced Data
# 1.1 A motivating example (wrong approach!)
m1 <- lm(area ~ CO2 + tree, stomata)
m0 <- lm(area ~ CO2, stomata)
anova(m0, m1)
# strong evidence for tree-to-tree variation, obscures effect of CO2

m2 <- lm(area ~ tree, stomata)
anova(m1, m2) # suggests tree only is the better model!

# (The right approach)
st <- aggregate(data.matrix(stomata), by=list(tree=stomata$tree), mean)
st$CO2 <- as.factor(st$CO2)
st
stomata
m3 <- lm(area ~ CO2, st)
anova(m3)

# between-tree variance
summary(m3)$sigma^2 - summary(m1)$sigma^2 / 4

# 1.3 A single random factor
head(Rail)
m1 <- lm(travel ~ Rail, Rail)
anova(m1)

rt <- aggregate(data.matrix(Rail), by=list(Rail$Rail), mean)
rt
m0 <- lm(travel ~ 1, rt)
sig <- summary(m1)$sigma
sigb <- (summary(m0)$sigma^2 - sig^2 / 3)^0.5
sigb
sig
summary(m0)

# 1.4 Model with two factors
head(Machines)
interaction.plot(Machines$Machine, Machines$Worker, Machines$score, lty=1, col=1:6)

m1 <- lm(score ~ Worker * Machine, Machines)
summary(m1)
m0 <- lm(score ~ Worker + Machine, Machines)
summary(m2)
anova(m0, m1)
# evidence for machine:worker interaction, so reject H: var[alpha, b] = 0
# Estimate var:
summary(m1)$sigma^2
# Examine main effects
Mach <- aggregate(
  data.matrix(Machines), by=list(Machines$Worker, Machines$Machine), mean)
Mach$Worker <- as.factor(Mach$Worker)
Mach$Machine <- as.factor(Mach$Machine)
m0 <- lm(score ~ Worker + Machine, Mach)
summary(m0)
anova(m0)
TukeyHSD(aov(m0))
# Interaction var:
summary(m0)$sigma^2 - summary(m1)$sigma^2 / 3
M <- aggregate(data.matrix(Mach), by=list(Mach$Worker), mean)
m00 <- lm(score ~ 1, M)
summary(m00)
mean(M$score)
# Worker var
summary(m00)$sigma^2 - (summary(m0)$sigma^2) / 3



# 4. Maximum Likelihood Estimation for the Linear Mixed Model
# 4.2 Maximizing the profile likelihood
llm <- function(theta , X, Z, y) {
  # Untransform params
  sigma.b <- exp(theta[1])
  sigma <- exp(theta[2])
  
  # Extract dims
  n <- length(y)
  pr <- ncol(Z)
  pf <- ncol(X)
  
  # Obtain beta.hat, b.hat
  X1 <- cbind(X, Z)
  ipsi <- c(rep(0, pf), rep(1 / sigma.b^2, pr))
  b1 <- solve(crossprod(X1)/sigma^2 + diag(ipsi), t(X1) %*% y/sigma^2)
  
  # Compute log|Z'Z/sigma^2 + I/sigma.b^2|
  ldet <- sum(log(diag(chol(crossprod(Z)/sigma^2 + diag(ipsi[-(1:pf)])))))
  
  # Compute log profile likelihood
  l <- ((-sum((y - X1 %*% b1)^2)/sigma^2 
         - sum(b1^2*ipsi) 
         - n*log(sigma^2) 
         - pr*log(sigma.b^2) 
         - 2*ldet 
         - n*log(2*pi)) 
        / 2)
  attr(l, 'b') <- as.numeric(b1) # return beta.hat, b.hat
  -l
}

options(contrasts=c('contr.treatment', 'contr.treatment'))
Z <- model.matrix(~ Rail$Rail - 1)
X <- matrix(1, 18, 1)
rail.mod <- optim(c(0, 0), llm, hessian=T, X=X, Z=Z, y=Rail$travel)
exp(rail.mod$par) # variance components
solve(rail.mod$hessian) # appx. cov matrix for theta
attr(llm(rail.mod$par, X, Z, Rail$travel), 'b')



# 5. Linear Mixed Models in R
# 5.1 Package nlme
head(Rail)
lme(travel ~ 1, data=Rail, random=list(Rail=~1)) # same as:
lme(travel ~ 1, Rail, ~1|Rail)

# 5.2 Tree growth: Example using lme
head(Loblolly)
Loblolly$age <- Loblolly$age - mean(Loblolly$age) # center

# Can control lme optimization params:
lmc <- lmeControl(niterEM=500, msMaxIter=100)
m0 <- lme(height ~ age + I(age^2) + I(age^3), 
          Loblolly, 
          random=list(Seed =~age + I(age^2) + I(age^3)), 
          correlation=corAR1(form=~age|Seed), 
          control=lmc)
summary(m0)
plot(m0)

m2 <- lme(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5), 
          Loblolly, 
          random=list(Seed =~age + I(age^2) + I(age^3)), 
          correlation=corAR1(form=~age|Seed), 
          control=lmc)
summary(m2)
plot(m2)

m3 <- lme(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5), 
          Loblolly, 
          random=list(Seed =~age + I(age^2) + I(age^3)), 
          control=lmc)
summary(m3)
plot(m3)
anova(m3, m2)

m4 <- lme(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5), 
          Loblolly, 
          random=list(Seed =~age + I(age^2)), 
          correlation=corAR1(form=~age|Seed), 
          control=lmc)
summary(m4)
anova(m4, m2)
plot(m4)

m5 <- lme(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5), 
          Loblolly, 
          list(Seed=pdDiag(~age + I(age^2) + I(age^3))),
          correlation=corAR1(form=~age|Seed), 
          control=lmc)
anova(m5, m2)

# m2 continues to be the better model
plot(m2)
plot(augPred(m2))

# 5.3 Several levels of nesting
lme(score ~ Machine, Machines, list(Worker=~1, Machine=~1))

# 5.4 Package lme4
a1 <- lmer(score ~ Machine + (1|Worker) + (1|Worker:Machine), data=Machines)
a1
a2 <- lmer(score ~ Machine + (1|Worker) + (Machine - 1|Worker), data=Machines)
a2
AIC(a1, a2)