#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(factoextra)
data(USArrests)



# 2. Steps to Agglomerative Hierarchical Clustering (AKA AgNes)
df <- scale(USArrests)
head(df)


# 2.2 Similarity Measures
# Dissimilarity matrix
res.dist <- dist(df, method='euclidean')
as.matrix(res.dist)[1:5, 1:5]


# 2.3 Linkage
res.hc <- hclust(d=res.dist, method='ward.D2')
# methods: ward.D, single, complete, average, mcquitty, median, centroid
# Ward minimizes within-cluster variance


# 2.4 Dendrogram
fviz_dend(res.hc, cex=0.5)



# 3. Verify the Cluster Tree