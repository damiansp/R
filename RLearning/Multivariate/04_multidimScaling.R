#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(ape)
library(HSAUR2)
library(MASS)
library(MVA)
data(skulls)
data(voting)
data(watervoles)

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

'airline.dist' <- structure(
  .Data=list(
    c(0, 587, 1212, 701, 1936, 604, 748, 2139, 2181, 543), 
    c(587, 0, 920, 940, 1745, 1188, 713, 1858, 1737, 597), 
    c(1212, 920, 0, 879, 831, 1726, 1631, 949, 1021, 1494),
    c(701, 940, 879, 0, 1374, 968, 1420, 1645, 1891, 1220),
    c(1936, 1745, 831, 1374, 0, 2339, 2451, 347, 959, 2300),
    c(604, 1188, 1726, 968, 2339, 0, 1092, 2594, 2734, 923),
    c(748, 713, 1631, 1420, 2451, 1092, 0, 2571, 2408, 205), 
    c(2139, 1858, 949, 1645, 347, 2594, 2571, 0, 678, 2442),
    c(218, 1737, 1021, 1891, 959, 2734, 2408, 678, 0, 2329),
    c(543, 597, 1494, 1220, 2300, 923, 205, 2442, 2329, 0)), 
  names=c("ATL", "ORD", "DEN", "HOU", "LAX", "MIA", "JFK", "SFO", "SEA", "IAD"),
  row.names=c("ATL", "ORD", "DEN", "HOU", "LAX", "MIA", "JFK", "SFO", "SEA", 
              "IAD"), 
  class="data.frame")
airdist <- as.dist(as.matrix(airline.dist))
#tmp <- airdist
#airdist <- as.data.frame(as.matrix(airdist))
#htab <- HSAURtable(airdist)
#htab$data[upper.tri(htab$data)] <- " "
#airdist <- tmp

airline.mds <- cmdscale(airdist, k=9, eig=T)
airline.mds$points
(lam <- airline.mds$eig)
cumsum(abs(lam)) / sum(abs(lam))
cumsum(lam^2) / sum(lam^2)
lim <- range(airline_mds$points[,1] * (-1)) * 1.2
plot(airline_mds$points[,1] * (-1), 
     airline_mds$points[,2] * (-1),
     type="n", 
     xlab="Coordinate 1", 
     ylab="Coordinate 2",
     xlim=lim, 
     ylim=lim)
text(airline_mds$points[,1] *(-1), 
     airline_mds$points[,2] * (-1), 
     labels(airdist), 
     cex=0.7)

# skull data
head(skulls)
skulls.var <- tapply(
  1:nrow(skulls), skulls$epoch, function(i) var(skulls[i, -1]))
S <- 0
for (v in skulls.var) {
  S <- S + 29 * v
}
(S <- S / 149)
skulls.cen <- tapply(
  1:nrow(skulls), skulls$epoch, function(i) apply(skulls[i, -1], 2, mean))
skulls.cen <- matrix(unlist(skulls.cen), nrow=length(skulls.cen), byrow=T)
skulls.mah <- apply(
  skulls.cen, 1, function(cen) mahalanobis(skulls.cen, cen, S))
skulls.mah
cmdscale(skulls.mah, k=nrow(skulls.mah) - 1, eig=T)$eig
skulls.mds <- cmdscale(skulls.mah)
lim <- range(skulls_mds) * 1.2
plot(skulls_mds, 
     xlab="Coordinate 1", 
     ylab="Coordinate 2",
     xlim=lim, 
     ylim=lim, 
     type="n")
text(skulls_mds, labels = levels(skulls$epoch), cex = 0.7)

head(watervoles)
voles.mds <- cmdscale(watervoles, k=13, eig=T)
voles.mds$eig
cumsum(abs(voles.mds$eig)) / sum(abs(voles.mds$eig))
cumsum(voles.mds$eig^2) / sum(voles.mds$eig^2)
x <- voles.mds$points[, 1]
y <- voles.mds$points[, 2]
plot(x, y, xlab='Coord 1', ylab='Coord 2', xlim=1.2 * range(x), type='n')
text(x, y, colnames(watervoles), cex=0.7)

st <- mst(watervoles) # min spanning tree
for (i in 1:nrow(watervoles)) {
  w1 <- which(st[i, ] == 1)
  segments(x[i], y[i], x[w1], y[w1], col=rgb(1, 0, 0, 0.5))
}



# 5. Non-metric Multidimensional Scaling
# 5.1 House of representative voting
voting.mds <- isoMDS(voting)
party <- ifelse(grepl('\\(R\\)', rownames(voting.mds$points)), 2, 4)
x <- voting.mds$points[, 1]
y <- voting.mds$points[, 2]
plot(x, 
     y, 
     xlab='Coord 1', 
     ylab='Coord 2', 
     xlim=range(voting.mds$points) * 1.2, 
     type='n')
text(x, y, colnames(voting), cex=0.6, col=party)