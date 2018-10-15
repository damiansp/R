#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)
data('USArrests')

df <- USArrests
df <- na.omit(df)
df <- scale(df)

head(df)


# 4. Distance Matrix Computation
# 4.1 Data preparation
ss <- sample(1:50, 15)
df <- USArrests[ss, ]
df.scaled <- scale(df)


# 4.3 Computing euclidean distance
dist.eucl <- dist(df.scaled, method='euclidean')
round(as.matrix(dist.eucl)[1:3, 1:3], 1)


# 4.4 Computing correlation-based distances
