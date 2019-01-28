#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')


library(factoextra)
library(clustertend)
library(NbClust)



# 4. Computing the Number of Clusters Using R


# 4.2 Data Prep
df <- scale(USArrests)


# 4.3 fviz_nbclust(): Elbow, Silhouette, and Gap Statistic Methods
# Elbow
(fviz_nbclust(df, kmeans, method='wss')  # within sum of squares
 + geom_vline(xintercept=4, linetype=2)
 + labs(subtitle='Elbow'))

# Silhouette
fviz_nbclust(df, kmeans, method='silhouette') + labs(subtitle='Silhouette')

# Gap
(fviz_nbclust(df, kmeans, nstart=25, method='gap_stat', nboot=500) 
 + labs(subtitle='Gap'))