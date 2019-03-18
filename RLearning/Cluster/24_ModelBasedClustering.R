#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

#library(cluster)
library(factoextra)
library(ggpubr)
library(MASS)
library(mclust)

data('diabetes')
data('geyser')



# 1 Concept of Model-Based Clustering
ggscatter(geyser, x='duration', y='waiting') + geom_density2d()



# 4 Computing Model-Based Clustering in R
head(diabetes)
ggscatter(diabetes, x='glucose', y='insulin') + geom_density2d()
ggscatter(diabetes, x='glucose', y='sspg') + geom_density2d()
df <- scale(diabetes[, -1])
mc <- Mclust(df)
summary(mc)
mc$modelName
mc$G # opt n of clusters
head(mc$z, 30) # affinity for each cluster
head(mc$classification, 30) 
ggscatter(diabetes, x='glucose', y='insulin', color=mc$classification)
ggscatter(diabetes, x='glucose', y='sspg', color=mc$classification)



# 5 Visualizing Model-Based Clustering
fviz_mclust(mc, 'BIC', palette='jco')
fviz_mclust(mc, 'classification', geom='point', pointsize=1.5, palette='jco')
fviz_mclust(mc, 'uncertainty', palette='jco')