#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')


#library(clValid)
#library(factoextra)
#library(fpc)
#library(NbClust)
library(parallel)
library(pvclust)



# 3 Data Prep
data('lung')
head(lung[, 1:5])
dim(lung)

samp <- sample(1:(dim(lung)[2]), 30)



# 4 Compute P-Value for Hierarchical Clustering


# 4.2 Usage of pvclust() function
res.pv <- pvclust(lung, method='cor', method.hclust='average', nboot=10)
plot(res.pv, hang=-1, cex=0.5)
pvrect(res.pv)
clusters <- pvpick(res.pv)
cl <- makeCluster(2, type='PSOCK')
#res.pv <- parPvclust(cl, lung, nboot=1000)