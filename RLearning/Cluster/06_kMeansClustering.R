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
