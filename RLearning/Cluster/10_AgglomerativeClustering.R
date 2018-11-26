#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
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
res.hc2 <- hclust(d=res.dist, method='average')
# methods: ward.D, single, complete, average, mcquitty, median, centroid
# Ward minimizes within-cluster variance


# 2.4 Dendrogram
fviz_dend(res.hc, cex=0.5)
fviz_dend(res.hc2, cex=0.5)



# 3. Verify the Cluster Tree
res.coph <- cophenetic(res.hc)
res.coph2 <- cophenetic(res.hc.avg)
cor(res.dist, res.coph) # 0.698 (the higher the better the clustering)
cor(res.dist, res.coph2) # 0.718 Better



# 4. Cut the Dendrogram into Different Groups
grp <- cutree(res.hc2, k=5)
head(grp)
table(grp)
fviz_dend(res.hc2, 
          k=5,
          cex=0.5,
          k_colors=c('#6e7fbf', '#00afbb', '#e7b800', '#fc4e07', '#1e6e7b'),
          color_lablel_by_k=T,
          rect=T)
fviz_cluster(list(data=df, cluster=grp),
             palette=c('#6e7fbf', '#00afbb', '#e7b800', '#fc4e07', '#1e6e7b'),
             ellipse.type='convex', # 't'
             repel=T,
             show.clust.cent=T)



# 5. Cluster R Package
res.agnes <- agnes(x=USArrests,
                   stand=T, # scale
                   metric='euclidean',
                   method='average')

# Divisive Anlysis Clustering
res.diana <- diana(x=USArrests,
                   stand=T,
                   metric='euclidean')
fviz_dend(res.agnes, cex=0.5, k=5)
fviz_dend(res.diana, cex=0.5, k=5)