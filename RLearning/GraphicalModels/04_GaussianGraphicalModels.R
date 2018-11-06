#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')

library(gRbase)
library(gRim)
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
