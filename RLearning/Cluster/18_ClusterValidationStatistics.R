#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')


library(factoextra)
library(fpc)
library(NbClust)



# 3 Computing Cluster Validation Statistics


# 3.2 Data preparation
df <- iris[, -5]
df <- scale(df)

# K-means
km.res <- eclust(df, 'kmeans', k=3, nstart=25, graph=F)
fviz_cluster(km.res, 
             geom='point', 
             ellipse.type='norm', 
             palette='jco', 
             ggtheme=theme_minimal())
# Hierarchical
hc.res <- eclust(
  df, 'hclust', k=3, hc_metric='euclidean', hc_method='ward.D2', graph=F)
fviz_dend(hc.res, show_labels=F, palette='jco', as.ggplot=T)


# 3.4 Clustering validation
# 3.4.1 Silhouette plot
