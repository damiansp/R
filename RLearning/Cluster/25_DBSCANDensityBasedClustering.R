#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(factoextra)

data('multishapes')



# 1. Why DBSCAN?
head(multishapes)
df <- multishapes[, 1:2]

km.res <- kmeans(df, 5, nstart=25)
fviz_cluster(km.res, 
             df, 
             ellipse=T, 
             ellipse.type='norm', 
             geom='point', 
             palette='jco', 
             ggtheme=theme_classic())
?fviz_cluster