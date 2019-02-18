#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')


library(clValid)
library(factoextra)
library(fpc)
library(NbClust)


# 2. Compare Clustering Algorithms in R
df <- scale(iris[, -5])
cl.methods <- c('hierarchical', 'kmeans', 'pam')
intern <- clValid(df, nClust=1:6, clMethods=cl.methods, validation='internal')
summary(intern)

stability <- clValid(df, nClust=1:6, clMethods=cl.methods, validation='stability')
summary(stability)
optimalScores(stability)