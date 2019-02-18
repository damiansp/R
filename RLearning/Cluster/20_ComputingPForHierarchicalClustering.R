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
library(pvclust)



# 3 Data Prep
data('lung')
head(lung[, 1:5])
dim(lung)

samp <- sample(1:(dim(lung)[2]), 30)



# 4 Compute P-Value for Hierarchical Clustering


# 4.1 Description of pvclust() function
