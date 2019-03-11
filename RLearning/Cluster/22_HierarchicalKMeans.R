#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(factoextra)


# 2 R Code
df <- scale(USArrests)
res.hk <- hkmeans(df, k=4)
names(res.hk)
res.hk
fviz_dend(res.hk, cex=0.6, palette='jco', rect=T, rect_border='jco', rect_fill=T)
fviz_cluster(res.hk, palette='jco', ellipse.type='euclid')