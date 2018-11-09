#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)

data(USArrests)


# 3. Computing PAM (Partitioning Aroud Medoids) in R 
# 3.1 Data
df <- scale(USArrests)


# 3.3 Estimating the Optimal Number of Clusters
fviz_nbclust(df, pam, method='silhouette')


# 3.4 Computing PAM Clustering
pam.res <- pam(df, 2, metric='euclidean')
pam.res
dd <- cbind(USArrests, cluster=pam.res$cluster)
head(dd)


# 3.5 Accessing the Results of the pam() Function
pam.res$medoids
head(pam.res$clustering)


# 3.6 Visualiing PAM Clusters
fviz_cluster(pam.res,
             palette=c('#9eaa00', '#3d06eb'),
             ellipse.type='t',
             repel=T)
