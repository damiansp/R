#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(ape)
library(HSAUR2)
library(MASS)
library(MVA)
#demo('Ch-MDS')
data(skulls)
data(teensex)
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


# 5.2 Judgements of WWII Leaders
x <- voting.mds$points[, 1]
y <- voting.mds$points[, 2]
plot(x, 
     y, 
     xlab='Coord 1', 
     ylab='Coord 2', 
     xlim=1.2 * range(voting.mds$points[, 1]),
     type='n')
text(x, y, labels=colnames(voting), cex=0.6)
WWII.leaders <- c(
  3,
  4, 6, 
  7, 8, 4, 
  3, 5, 6, 8,
  8, 9, 3, 9, 8, 
  3, 2, 5, 7, 6, 7,
  4, 4, 3, 5, 6, 5, 4,  
  8, 9, 8, 9, 6, 9, 8, 7,       
  9, 9, 5, 4, 7, 8, 8, 4, 4,
  4, 5, 5, 4, 7, 2, 2, 5, 9, 5,
  7, 8, 2, 4, 7, 8, 3, 2, 4, 5, 7)
tmp <- matrix(0, ncol=12, nrow=12)
tmp[upper.tri(tmp)] <- WWII.leaders
tmp <- tmp + t(tmp)
#rownames(tmp) <- colnames(tmp) <- c(
#  "Hitler", "Mussolini", "Churchill", "Eisenhower", "Stalin", "Attlee", 
#  "Franco", "De Gaulle", "Mao Tse-Tung", "Truman", "Chamberlin", "Tito")
#WWII.leaders <- as.dist(tmp)
#WWII.leaders <- as.data.frame(as.matrix(WWII.leaders))
#colnames(WWII.leaders) <- abbreviate(colnames(WWII.leaders), 3)]
WWII.leaders <- tmp
(WWII.mds <- isoMDS(WWII.leaders))
voting.sh <- Shepard(voting[lower.tri(voting)], voting.mds$points)


plot(voting.sh, 
     xlab='Dissimilarity', 
     ylab='Distance', 
     xlim=range(voting.sh$x), 
     ylim=range(voting.sh$x))
lines(voting.sh$x, voting.sh$yf, type='S', col=2)



# 6. Correspondence Analysis
# 6.1 Teenage relationships

teensex <- matrix(c(21, 8, 2, 21, 9, 3, 14, 6, 4, 13, 8, 10, 8, 2, 10), nrow=3)
rownames(teensex) <- c("No boyfriend", "Boyfriend no sex", "Boyfriend sex")
colnames(teensex) <- c("<16", "16-17", "17-18", "18-19", "19-20")
teensex <- as.table(teensex, dnn=c("Boyfriend", "Age group"))
#teensex <- as.data.frame(teensex)
#names(teensex) <- c("Boyfriend", "Age", "Freq")


dist.chisq <- function(x) {
  a <- t(t(x) / colSums(x))
  rep1 <- rep(1:ncol(x), ncol(x))
  rep2 <- rep(1:ncol(x), rep(ncol(x), ncol(x)))
  ret <- sqrt(colSums((a[, rep1] - a[, rep2])^2 * sum(x) / rowSums(x)))
  matrix(ret, ncol=ncol(x))
}

(dcols <- dist.chisq(teensex))
(drows <- dist.chisq(t(teensex)))
r1 <- cmdscale(dcols, eig=T)
c1 <- cmdscale(drows, eig=T)
plot(r1$points, 
     xlim=range(r1$points[,1], c1$points[,1]) * 1.5,
     ylim=range(r1$points[,1], c1$points[,1]) * 1.5, 
     type="n",  
     xlab="Coordinate 1", 
     ylab="Coordinate 2", 
     lwd=2)
text(r1$points, labels=colnames(teensex), cex=0.7)
text(c1$points, labels=rownames(teensex), cex=0.7)
abline(h=0, lty=2)
abline(v=0, lty=2)

