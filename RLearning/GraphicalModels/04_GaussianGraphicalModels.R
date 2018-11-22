#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')

library(gRbase)
library(gRim)
library(pcalg)
library(RBGL)
library(Rgraphviz)

data(BodyFat)
data(carcass)



# 2. Some Examples
# 2.1 Carcass Data
head(carcass)
S.carc <- cov.wt(carcass, method='ML')$cov # covariance matrix
K.carc <- solve(S.carc)                    # inverse
round(100 * K.carc)

PC.carc <- cov2pcor(S.carc) # partial covariance
round(100 * PC.carc)

sat.carc <- cmod(~ .^., data=carcass)
aic.carc <- stepwise(sat.carc)
plot(as(aic.carc, 'graphNEL'), 'fdp')

# BIC penalizes complexity more
bic.carc <- stepwise(sat.carc, k=log(nrow(carcass)))
bic.carc
plot(as(bic.carc, 'graphNEL'), 'fdp')


# 2.2 Body Fat Data
head(BodyFat)
plot(1 / BodyFat$Density, BodyFat$BodyFat)

# Remove obvious errors
BodyFat <- BodyFat[-c(31, 42, 48, 76, 86, 96, 159, 169, 175, 182, 206), ]
plot(1 / BodyFat$Density, BodyFat$BodyFat)

BodyFat$Age <- sqrt(BodyFat$Age)
BodyFat$Weight <- sqrt(BodyFat$Weight)
gRbodyfat <- BodyFat[, 2:15]

# Construct partial correlation matrix
S.body <- cov.wt(gRbodyfat, method='ML')$cov
PC.body <- cov2pcor(S.body)
round(100 * PC.body)

sat.body <- cmod(~ .^., data=gRbodyfat)
bic.body <- stepwise(sat.body, k=log(nrow(gRbodyfat)))
bic.body

graph::degree(as(bic.body, 'graphNEL'))
plot(as(bic.body, 'graphNEL'))



# 3. Undirected Gaussian Graphical Models
# 3.1 Preliminaries and Notation
# model with edges missing if partial cor < 0.12:
gen.carc <- cmod(~Fat11*Fat12*Meat12*Meat13 
                   + Fat11*Fat12*Fat13*LeanMeat
                   + Meat11*Meat12*Meat13
                   + Meat11*Fat13*LeanMeat,
                 data=carcass)
gen.carc
plot(gen.carc, 'neato')

edge.carc <- cmod(edgeList(as(gen.carc, 'graphNEL')), data=carcass)
edge.carc
plot(edge.carc) # same as prev


# 3.2 Estimation, Likelihood, and Model Fitting
carc.fit1 <- ggmfit(S.carc, n=nrow(carcass), edgeList(as(gen.carc, 'graphNEL')))
carc.fit1 # see esp, dev, df, iter

# More efficient to specify cliques of graph:
cgens <- maxClique(as(gen.carc, 'graphNEL'))$maxCliques
carc.fit2 <- ggmfit(S.carc, n=nrow(carcass), glist=cgens)
carc.fit2 # same but only 61 iters vs 774


# 3.3 Hypothesis Testing
compare.models <- function(m1, m2) {
  lrt <- m2$fitinfo$dev - m1$fitinfo$dev
  df.diff <- m2$fitinfo$dimension[4] - m1$fitinfo$dimension[4]
  names(df.diff) <- NULL
  list(likelihood.ratio.test=lrt, df=df.diff)
}

compare.models(aic.carc, bic.carc) # Large LRT suggest null hypothesis is false
# lrt = 8.4 indicating null hypothesis is false; prefer the more complicated
# model

# Test if a single edge could be deleted from model, e.g test:
# LeanMeat ind of Meat13 given rest
ciTest_mvn(list(cov=S.carc, n.obs=nrow(carcass)), 
           set=~LeanMeat+Meat13+Meat11+Meat12+Fat11+Fat12+Fat13)
ciTest_mvn(list(cov=S.carc, n.obs=nrow(carcass)), 
           set=~LeanMeat+Meat11+Meat12+Meat13+Fat11+Fat12+Fat13,
           statistic='F')
C.carc <- cov2cor(S.carc)
gaussCItest(7, 2, c(1, 3, 4, 5, 6), list(C=C.carc, n=nrow(carcass)))
# nearly same as previous


# 3.4 Concentration and Regression
# Regression coef for predicting LeanMeat:
-K.carc[7, -7] / K.carc[7, 7]
# and resid var of lean meat percentage is
1 / K.carc[7, 7]

r.LeanMeat <- residuals(lm(LeanMeat ~ Meat11 + Meat13 + Fat11 + Fat12 + Fat13,
                           data=carcass))
r.Meat12 <- residuals(lm(Meat12 ~ Meat11 + Meat13 + Fat11 + Fat12 + Fat13, 
                         data=carcass))
plot(r.LeanMeat ~ r.Meat12)
abline(h=0, col='grey')


# 3.5 Decomposition of UGGMs
K.hat <- S.carc
K.hat[] <- 0 # sets all values to 0!
AC <- c('Fat11', 'Fat12', 'Fat13', 'Meat11', 'LeanMeat')
BC <- c('Meat11', 'Meat12', 'Meat13', 'Fat11', 'Fat12')
C <- c('Fat11', 'Fat12', 'Meat11')
K.hat[AC, AC] <- K.hat[AC, AC] + solve(S.carc[AC, AC])
K.hat[BC, BC] <- K.hat[BC, BC] + solve(S.carc[BC, BC])
K.hat[C, C] <- K.hat[C, C] - solve(S.carc[C, C])
round(100 * K.hat)

Sigma.hat <- solve(K.hat)
round(Sigma.hat, 2)
round(S.carc, 2)



# 4 Model Selection
test.carc <- stepwise(sat.carc, details=1, 'test')
plot(test.carc, 'neato')