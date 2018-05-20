#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(MVA)
data('USairpollution')

measure <- read.csv('data/measure.csv')



# 5. Covariances, Correlation and Distances
# 5.1 Covariances
cov(measure[, c('chest', 'waist', 'hips')])
cov(subset(measure, gender == 'female')[, c('chest', 'waist', 'hips')])
cov(subset(measure, gender == 'male')[, c('chest', 'waist', 'hips')])

# 5.2 Correlations
cor(measure[, c('chest', 'waist', 'hips')])
cor(subset(measure, gender == 'female')[, c('chest', 'waist', 'hips')])
cor(subset(measure, gender == 'male')[, c('chest', 'waist', 'hips')])

# 5.3 Distances
dim(measure)
dist(scale(measure[, c('chest', 'waist', 'hips')], center=F))



# 6. The Multivariate Normal Density Function
x <- measure[, c('chest', 'waist', 'hips')]
cm <- colMeans(x)
S <- cov(x)

# distances from the mean vector
vector.dist <- function(x) { t(x - cm) %*% solve(S) %*% (x - cm) }
d <- apply(x, 1, vector.dist)
par(mfrow=c(1, 3))
for (m in c('chest', 'waist', 'hips')) {
  qqnorm(measure[, m], main=m)
  qqline(measure[, m])
}

par(mfrow=c(1, 1))
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df=3), 
     sort(d),
     xlab=expression(paste(chi[3]^2, ' Quantile')),
     ylab='Ordered distances')
abline(0, 1)

par(mfrow=c(3, 3))
sapply(
  colnames(USairpollution), 
  function(x) {
    qqnorm(USairpollution[[x]], main=x)
    qqline(USairpollution[[x]])
  })
  
usa <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, vector.dist)
qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df=6)
dist.sorted <- sort(d)
plot(qc, 
     dist.sorted,
     xlim=c(0, 20),
     xlab=expression(paste(chi[3]^2, ' Quantile')),
     ylab='Ordered distances')
outliers <- which(rank(abs(qc - dist.sorted), ties='random') > nrow(x) - 3)
text(qc[outliers] + 1.5, dist.sorted[outliers] - 1.5, names(outliers))
abline(0, 1)