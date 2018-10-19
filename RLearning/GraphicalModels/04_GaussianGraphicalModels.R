#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')

library(gRbase)
library(Rgraphviz)

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
