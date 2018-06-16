#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library('MVA')


# 4. Should PCs be Extracted from Covariance or Correlation Matrix?
# Really no reason you should not always use correlations -- scale invariant
blood.sd <- c(rblood=0.371, plate=41.253,  wblood=1.935, neut=0.077, lymph=0.071, 
              bilir=4.037, sodium=2.732, potass=0.297)
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
  191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190, 188, 163, 
  195, 186, 181, 175, 192, 174, 176, 197, 190, 155, 149, 148, 153, 144, 157, 150, 
  159, 152, 150, 158, 147, 150, 159, 151, 137, 155, 153, 145, 140, 154, 143, 139, 
  167, 163, 179, 201, 185, 188, 171, 192, 190, 189, 197, 187, 186, 174, 185, 195,
  187, 161, 183, 173, 182, 165, 185, 178, 176, 200, 187, 145, 152, 149, 149, 142, 
  152, 149, 152, 159, 151, 148, 147, 152, 157, 158, 130, 158, 148, 146, 137, 152, 
  147, 143, 158, 150), 
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
