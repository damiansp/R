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
fviz_silhouette(km.res, palette='jco', ggtheme=theme_classic())
sil.info <- km.res$silinfo
names(sil.info)
head(sil.info$widths)
sil.info$clus.avg.widths
sil.info$avg.width
km.res$size
sil <- km.res$silinfo$widths[, 1:3]

# Objects with negative silhouette (likely misclassified)
neg.sil.index <- which(sil[, 'sil_width'] < 0)
sil[neg.sil.index, , drop=F]

# 3.4.2 Validation Statistics
km.stats <- cluster.stats(dist(df), km.res$cluster)
km.stats
km.stats$dunn


# 3.5 External Clustering Validation
table(iris$Species, km.res$cluster)
species <- as.numeric(iris$Species)
clust.stats <- cluster.stats(d=dist(df), species, km.res$cluster)
clust.stats$corrected.rand # 0.620
clust.stats$vi             # 0.748

# Same analysis for PAM and hierarchical clustering:
pam.res <- eclust(df, 'pam', k=3, graph=F)
table(iris$Species, pam.res$cluster)
cluster.stats(d=dist(df), species, pam.res$cluster) # crand 0.642, vi 0.713

res.hc <- eclust(df, 'hclust', k=3, graph=F)
table(iris$Species, res.hc$cluster)
cluster.stats(d=dist(df), species, res.hc$cluster) $ # 0.615, 0.694