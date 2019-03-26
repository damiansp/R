#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(dbscan)
library(factoextra)
library(fpc)

data('multishapes', package='factoextra')



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



# 5. Computing DBSCAN
db <- fpc::dbscan(df, eps=0.14, MinPts=5)
fviz_cluster(db, 
             data=df, 
             stand=F, 
             ellipse=T, 
             ellipse.type='norm', 
             geom='point', 
             palette='jco')
db
db$cluster # oultiers are 0



# 6. Optimizing Epsilon
dbscan::kNNdistplot(df, k=5)
abline(h=0.14, col=2)

dbscan::kNNdistplot(df, k=8)
abline(h=0.16, col=2)
db <- fpc::dbscan(df, eps=0.16, MinPts=8)
fviz_cluster(db, 
             data=df, 
             stand=F, 
             ellipse=T, 
             ellipse.type='norm', 
             geom='point', 
             palette='jco')
