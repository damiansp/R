#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(HSAUR2)
library(MVA)
data('heptathlon')
#demo('Ch-PCA')

# 4. Should PCs be Extracted from Covariance or Correlation Matrix?
# Really no reason you should not always use correlations -- scale invariant
blood.sd <- c(rblood=0.371, plate=41.253,  wblood=1.935, neut=0.077, 
              lymph=0.071, bilir=4.037, sodium=2.732, potass=0.297)
bc <- c(
  0.290,           
  0.202,  0.415,       
 -0.055,  0.285,  0.419,       
 -0.105, -0.376, -0.521, -0.877,      
 -0.252, -0.349, -0.441, -0.076,  0.206,
 -0.229, -0.164, -0.145,  0.023,  0.034,  0.192,
  0.058, -0.129, -0.076, -0.131,  0.151,  0.077,  0.423)
blood.corr <- diag(length(blood.sd)) / 2
blood.corr[upper.tri(blood.corr)] <- bc
blood.corr <- blood.corr + t(blood.corr)
blood.cov <- blood.corr * outer(blood.sd, blood.sd, '*')

blood.pca.cov <- princomp(covmat=blood.cov)
summary(blood.pca.cov, loadings=T)

blood.pca.cor <- princomp(covmat=blood.corr)
summary(blood.pca.cor, loadings=T)



# 9 Calculating Principal Components Scores
plot(blood.pca.cor)
plot(blood.pca.cor$sdev^2, 
     xlab='Component No.', 
     ylab='Component Variance', 
     type='l', 
     main='Scree Diagram')
plot(log(blood.pca.cor$sdev^2), 
     xlab='Component No.', 
     ylab='log(comp var)', 
     type='l', 
     main='log(eigenvalue) Diagram')     



# 10 Some Examples of the Application of PCA
headsize <- matrix(c(
  191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190, 188, 
  163, 195, 186, 181, 175, 192, 174, 176, 197, 190, 155, 149, 148, 153, 144, 
  157, 150, 159, 152, 150, 158, 147, 150, 159, 151, 137, 155, 153, 145, 140, 
  154, 143, 139, 167, 163, 179, 201, 185, 188, 171, 192, 190, 189, 197, 187, 
  186, 174, 185, 195, 187, 161, 183, 173, 182, 165, 185, 178, 176, 200, 187, 
  145, 152, 149, 149, 142, 152, 149, 152, 159, 151, 148, 147, 152, 157, 158, 
  130, 158, 148, 146, 137, 152, 147, 143, 158, 150), 
  nrow=25, 
  ncol=4,  
  dimnames=list(character(0), c("head1", "breadth1", "head2", "breadth2")))
headsize <- as.data.frame(headsize)
head.dat <- headsize[, c('head1', 'head2')]
colMeans(head.dat)
cov(head.dat)
cor(head.dat)
head.pca <- princomp(x=head.dat)
head.pca
plot(head.pca)
print(summary(head.pca), loadings=T)


a1 <- mean(head.dat$head2) - 0.72 * mean(head.dat$head1) / 0.69
b1 <- 0.72 / 0.69
a2 <- mean(head.dat$head2) - (-0.69 * mean(head.dat$head1) / 0.72) 
b2 <- -0.69 / 0.72
plot(head.dat, xlab='Head length of son 1 (mm)', ylab='Head length of son 2 (mm)')
abline(a1, b1) # PC1
abline(a2, b2) # PC2
xl <- range(head.pca$scores[, 1])
plot(head.pca$scores, xlim=xl, ylim=xl)

# 10.2 Olympic heptathlon results
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m
score <- which(colnames(heptathlon) == 'score')
round(cor(heptathlon[, -score]), 2)
plot(heptathlon[, -score])

heptathlon <- heptathlon[-grep('PNG', rownames(heptathlon)), ]
round(cor(heptathlon[, -score]), 2)
plot(heptathlon[, -score])

heptathlon.pca <- prcomp(heptathlon[, -score], scale=T)
print(heptathlon.pca) # loadings
summary(heptathlon.pca)
a1 <- heptathlon.pca$rotation[, 1]
a1
center <- heptathlon.pca$center
scale <- heptathlon.pca$scale
heptathlon.m <- as.matrix(heptathlon[, -score])

# Each data point's projection on 1st PC:
drop(scale(heptathlon.m, center=center, scale=scale) 
     %*% heptathlon.pca$rotation[, 1])

# Same as:
predict(heptathlon.pca)[, 1]
plot(heptathlon.pca)
cor(heptathlon$score, heptathlon.pca$x[, 1])
plot(heptathlon$score, heptathlon.pca$x[, 1])

plot(heptathlon.pca$x[,1], 
     heptathlon.pca$x[,2], 
     type='n', 
     xlim=c(-5, 5), 
     ylim=c(-2, 1))
text(heptathlon.pca$x[,1], heptathlon.pca$x[,2], rownames(heptathlon.pca$x))



# 10.3 Air pollution in US cities
cor(USairpollution[, -1])
usair.pca <- princomp(USairpollution[, -1], cor=T)
panel.hist <- function(x, ...) {
  usr <- par('usr')
  on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=F)
  breaks <- h$breaks
  n.breaks <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-n.breaks], 0, breaks[-1], y, ...)
}

USairpollution$negtemp <- -1 * USairpollution$temp
USairpollution$temp <- NULL
pairs(
  USairpollution[, -1], diag.panel=panel.hist, pch=16, col=rgb(0, 0, 0, 0.5))
  
summary(usair.pca, loadings=T)

pairs(
  usair.pca$scores[, 1:3], 
  ylim=c(-6, 4), 
  xlim=c(-6, 4), 
  panel=function(x, y, ... ) {
    text(x, y, abbreviate(row.names(USairpollution)), cex=0.6)
    bvbox(cbind(x, y), add=T)
  })

par(mfrow=c(3, 2))  
out <- sapply(
  1:6, 
  function(i) {
    plot(USairpollution$SO2, usair.pca$scores[, i],
         xlab=paste('PC', i, sep=''),
         ylab='SO2 concentration')
  })
  
usair.reg <- lm(SO2 ~ usair.pca$scores, data=USairpollution)
summary(usair.reg)



# 11. The Biplot
par(mfrow=c(1, 1))
biplot(heptathlon.pca, col=1:2, xlim=c(-0.6, 0.5))



# 13. Canonical Correlation Analysis
headsize.std <- sweep(headsize, 2, apply(headsize, 2, sd), FUN='/')
R <- cor(headsize.std)
r11 <- R[1:2, 1:2]
r22 <- R[-(1:2), -(1:2)]
r12 <- R[1:2, -(1:2)]
r21 <- R[-(1:2), 1:2]
(E1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21)
(E2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12)
(e1 <- eigen(E1))
(e2 <- eigen(E2))
girth1 <- headsize.std[, 1:2] %*% e1$vectors[, 1]
girth2 <- headsize.std[, 3:4] %*% e2$vectors[, 1]
shape1 <- headsize.std[, 1:2] %*% e1$vectors[, 2]
shape2 <- headsize.std[, 3:4] %*% e2$vectors[, 2]