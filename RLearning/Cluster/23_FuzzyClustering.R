#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)



# 2 Computing Fuzzy Clustering
df <- scale(USArrests)
res.fanny <-fanny(df, 2) # fuzzy analysis
head(res.fanny$membership)
res.fanny$coef # Dunn's partition coef # 0.1095
head(res.fanny$clustering)
fviz_cluster(res.fanny, ellipse.type='norm', repel=T, palette='jco')
fviz_silhouette(res.fanny, palett='jco', ggtheme=theme_minimal()) # 0.39