#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)
data(USArrests)

res.diana <- diana(USArrests, stand=T)
fviz_dend(res.diana, cex=0.5, k=5, palette='jco')