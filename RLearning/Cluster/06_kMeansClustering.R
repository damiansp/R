#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

#library(cluster)
library(factoextra)

data(USArrests)
#data(flower)




# 3 Computing k-Means Clusterin in R
# 3.1 Data
df <- scale(USArrests)
head(df)


# 3.3 Estimating the optimal number of clusters
fviz_nbclust(df, kmeans, method='wss') + geom_vline(xintercept=4, lty=2)


# 3.4 Computing k-means clustering
set.seed(456)
km.res <- kmeans(df, 4, nstart=25)
km.res
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
(dd <- cbind(USArrests, cluster=km.res$cluster))


# 3.5 Accessing the results of kmeans()
# attributes: clusters, centers, totss, withinss, tot.withinss, betweenss, size
km.res$cluster
km.res$size
km.res$centers


# 3.6 Visualizing k-means clusters