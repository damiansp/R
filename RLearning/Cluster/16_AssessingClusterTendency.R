#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')


library(factoextra)
library(clustertend)



# 2. Data Prep
head(iris)
df <- iris[, -5]
random.df <- as.data.frame(
  apply(df, 2, function(x) runif(length(x), min(x), max(x))))
df <- iris.scaled <- scale(df)
romdom.df <- scale(random.df)



# 3. Visual Inpsection
fviz_pca_ind(prcomp(df),
             title='PCA of Iris Data',
             habillage=iris$Species,
             palette='jco',
             geom='point',
             ggtheme=theme_classic(),
             legend='bottom')
fviz_pca_ind(prcomp(random.df),
             title='PCA of Random Data',
             geom='point',
             ggtheme=theme_classic())



# 4. Why to Asses Clustering Tendency
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data=df, cluster=km.res1$cluster), 
             ellipse.type='norm', 
             geom='point', 
             stand=F, 
             palette='jco',
             ggtheme=theme_classic())
km.res2 <- kmeans(random.df, 3)
fviz_cluster(list(data=random.df, cluster=km.res1$cluster), 
             ellipse.type='norm', 
             geom='point', 
             stand=F, 
             palette='jco',
             ggtheme=theme_classic())
fviz_dend(hclust(dist(random.df)), k=3, k_colors='jco', as.ggplot=T, show_labels=F)
fviz_dend(hclust(dist(df)), k=3, k_colors='jco', as.ggplot=T, show_labels=F)



# 5. Methods for Assessing Clustering Tendency


# 5.1 Statistical Methods
res <- get_clust_tendency(df, n=nrow(df) - 1, graph=F)
res$hopkins_stat # signif at 90CI if > 0.75
res.rand <- get_clust_tendency(random.df, n=nrow(random.df) - 1, graph=F)
res.rand$hopkins_stat


# 5.2 Visual Methods
fviz_dist(dist(df), show_labels=F) + labs(title='Iris Data')
fviz_dist(dist(random.df), show_labels=F) + labs(title='Random Data')