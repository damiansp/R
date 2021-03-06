#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')

library(ggm)
library(gRbase)
library(gRim)
library(igraph)
#library(lcd) # no version for 3.5
library(RBGL)
library(Rgraphviz)
library(sna)
data(dumping)
data(lizard)
data(lizardAGG)
data(lizardRAW)
data(mildew)
data(reinis)



# 2. Preliminaries
# 2.1 Four data sets
str(reinis)
str(mildew)
str(dumping)


# 2.2 Data Formats
head(lizardRAW)
lizardAGG
lizard



# 3. Log-Linear Models
# 3.2 Hierarchical log-linear models
m1 <- dmod(~species*height + species*diam, data=lizard) # same as
m2 <- dmod(list(c('species', 'height'), c('species', 'diam')), lizard)
plot(m1) # height indep of diam given species

# which conditional independences hold over model?
separates('height', 'diam', 'species', as(m1, 'graphNEL')) # T


# 3.3 Graphical and decomposable log-linear models
no3f <- dmod(~species:height + species:diam + height:diam, data=lizard)
par(mfcol=c(1, 2))
sat <- dmod(~species:height:diam, data=lizard)
plot(no3f, main='no 3-factor interaction')
plot(sat, main='saturated')
summary(no3f)

g <- ug(~la10:locc:mp58 + locc:mp58:c365 + mp58:c365:p53a + c365:p53a:a367)
mg <- dmod(g, data=mildew)
plot(mg)
summary(mg) # decomposable: has closed for for ML estimation



# 3.5 Hypothesis testing
m1 <- dmod(~species:height + species:diam, data=lizard)
plot(m1)
m1
m1$fitinfo$pearson

m3 <- dmod(~la10*locc*mp58*c365*p53a + locc*mp58*c365*p53a*a367, data=mildew)
m4 <- update(m3, list(dedge=~locc*a367))
par(mfrow=c(1, 2))
plot(m3, 'neato')
plot(m4, 'neato')

compare.models <- function(m1, m2) {
  lrt <- m2$fitinfo$dev - m1$fitinfo$dev
  df.diff <- m1$fitinfo$dimension[1] - m2$fitinfo$dimension[1]
  c('lrt'=lrt, 'df'=df.diff)  
}

m3
m4
compare.models(m3, m4)
testdelete(m3, edge=c('locc', 'a367')) # same

# Test for condit indep
cit <- ciTest_table(mildew, set=c('locc', 'a367', 'mp58', 'c365', 'p53a'))
cit2 <- ciTest_table(mildew, 
                     set=c('locc', 'a367', 'mp58', 'c365', 'p53a', 'la10'))
cit$slice

cit <- ciTest_table(
  mildew, set=c('locc', 'a367', 'mp58', 'c365', 'p53a'), method='MC')
ciTest_ordinal(dumping, c(2, 1, 3), 'jt', N=1000) # arg2 is df columns; N for MC
ciTest_ordinal(dumping, c(3, 1, 2), 'kruskal', N=1000)



# 4. Model Selection
m.init <- dmod(~.^., data=reinis) # ^.: fully connected
plot(m.init)
m.reinis <- stepwise(m.init) # AIC
plot(m.reinis)
m.reinis2 <- stepwise(m.init, k=log(sum(reinis))) # BIC
plot(m.reinis2)

mildew.init <- dmod(~.^1, data=mildew) # ^1: unconnected 
plot(mildew.init)
m.mildew <- stepwise(
  mildew.init, k=log(sum(mildew)), direction='forward', details=1)
plot(m.mildew)

mildew.init.2 <- dmod(~.^., data=mildew)
m.mildew.2 <- stepwise(mildew.init.2, crit='test', alpha=0.05, details=0)
m.mildew.2
plot(m.mildew.2)



# 5. Further Topics
# 5.1 Fitting log-linear models with glm()
lizardAGG
m1.glm <- glm(
  Freq ~ 1 + diam:species + height:species, family=poisson, data=lizardAGG)
summary(m1.glm)

mild.glm <- glm(Freq ~ .^3, family=poisson, data=as.data.frame(mildew))
summary(mild.glm)

mildew.dmod <- dmod(~ .^3, data=mildew)
plot(mildew.dmod)
mildew.step <- stepwise(mildew.dmod)
summary(mildew.step)
plot(mildew.step)


# 5.2 Working with dModel objects
m <- dmod(~ .^2, marginal=c('smo', 'prot', 'sys', 'fam'), data=reinis)
as(m, 'graphNEL')
plot(m)
as(m, 'matrix')