#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)

#data(USArrests)



# 3. Computing CLARA in R
# 3.1 Data Format and Prep
df <- rbind(cbind(rnorm(200,  0,  8), rnorm(200,  0, 8)),
            cbind(rnorm(200, 50,  8), rnorm(200, 50, 8)),
            cbind(rnorm(200, 81,  8), rnorm(200, 81, 8)))
df <- data.frame(df)
colnames(df) <- c('x', 'y')
df$actual <- rep(c(1, 2, 3), each=200)
rownames(df) <- paste('S', 1:nrow(df), sep='')
head(df)


# 3.3 Estimating the Optimal Number of Clusters
fviz_nbclust(df, clara, method='silhouette')


# 3.4 Computing CLARA
clara.res <- clara(df, 3, samples=50, pamLike=T)
clara.res
dd <- cbind(df, cluster=clara.res$cluster)
head(dd)
clara.res$medoids

table(dd$actual, dd$cluster)


# 3.5 Viusualization
fviz_cluster(clara.res,
             palette=c('#bb00af', '#07fc4e', '#00afbb'),
             ellipse.type='t',
             geom='point',
             pointsize=1)