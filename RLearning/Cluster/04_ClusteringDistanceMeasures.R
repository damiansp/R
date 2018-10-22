#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)
data(USArrests)
data(flower)

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
dist.cor <- get_dist(df.scaled, method='pearson')
round(as.matrix(dist.cor)[1:3, 1:3], 2)


# 4.5 Computing distances for mixed data
head(flower)
str(flower) # note some factors and ordered factors
dd <- daisy(flower)
round(as.matrix(dd)[1:5, 1:5], 2)



# 5. Visualizing Distance Matrices
fviz_dist(dist.eucl)
fviz_dist(dist.cor)