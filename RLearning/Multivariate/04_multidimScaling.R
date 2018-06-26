#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(MVA)


# 4. Classical Multidimensional Scaling
# 4.2 Examples 
X <- matrix(c(3, 4, 4, 6, 1,
              5, 1, 1, 7, 3,
              6, 2, 0, 2, 6,
              1, 1, 1, 0, 3,
              4, 7, 3, 6, 2,
              2, 2, 5, 1, 0,
              0, 4, 1, 1, 1,
              0, 6, 4, 3, 5,
              7, 6, 5, 1, 4,
              2, 1, 4, 3, 1),
            nrow=10,
            byrow=F)
(D <- dist(X))
cmdscale(D, k=9, eig=T) 
# if able to fully recover dist matrix, the following should be ~0
max(abs(dist(X) - dist(cmdscale(D, k=5))))
# cf PCA
max(abs(prcomp(X)$x) - abs(cmdscale(D, k=5)))

# Non-Euclidean
X.m <- cmdscale(dist(X, method='manhattan'), k=nrow(X) - 1, eig=T)
(X.eigen <- X.m$eig)
cumsum(abs(X.eigen)) / sum(abs(X.eigen))
plot(cumsum(abs(X.eigen)) / sum(abs(X.eigen)), type='l')
cumsum(X.eigen^2) / sum(X.eigen^2)
plot(cumsum(X.eigen^2) / sum(X.eigen^2), type='l')
# both suggest 3D solution is good

airline.mds <- cmdscale(airdist, k=9, eig=T)