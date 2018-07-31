#=========#=========#=========#=========#=========#=========#=========#=========
setwd('~/Learning/R/RLearning/Multivariate')

library(MVA)
library(mvtnorm)
#demo('Ch-CA')


measure <- structure(
  list(
    V1=1:20, 
    V2=c(34L, 37L, 38L, 36L, 38L, 43L, 40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 
         36L, 37L, 34L, 36L, 38L, 35L), 
    V3=c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L, 24L, 25L, 24L, 22L, 
         26L, 26L, 25L, 26L, 28L, 23L), 
    V4=c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L, 
         38L, 37L, 38L, 37L, 40L, 35L)), 
  .Names=c("V1", "V2", "V3", "V4"), 
  class="data.frame", 
  row.names=c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

dat <- rbind(rmvnorm(25, mean=c(3, 3)), 
             rmvnorm(20, mean=c(10, 8)), 
             rmvnorm(10, mean=c(20, 1)))
             
dm <- dist(measure[, c("chest", "waist", "hips")])
round(dm, 2)

plot(cs <- hclust(dm, method='single'))
plot(cs <- hclust(dm, method='complete'))
plot(cs <- hclust(dm, method='average'))

body.pc <- princomp(dm, cor=T)
x.lim <- range(body.pc$scores[, 1])
plot(body.pc$scores[, 1:2], pch=16, col=cutree(cs, h=7.5))
plot(body.pc$scores[, 1:2], pch=16, col=cutree(cs, h=6), asp=1)


jet <- structure(
  list(
    V1=c(82L, 89L, 101L, 107L, 115L, 122L, 127L, 137L, 147L, 166L, 174L, 175L, 
         177L, 184L, 187L, 189L, 194L, 197L, 201L, 204L, 255L, 328L), 
    V2=c(1.468, 1.605, 2.168, 2.054, 2.467, 1.294, 2.183, 2.426, 2.607, 4.567, 
         4.588, 3.618, 5.855, 2.898, 3.88, 0.455, 8.088, 6.502, 6.081, 7.105, 
         8.548, 6.321), 
    V3=c(3.3, 3.64, 4.87, 4.72, 4.11, 3.75, 3.97, 4.65, 3.84, 4.92, 3.82, 4.32,
         4.53, 4.48, 5.39, 4.99, 4.5, 5.2, 5.65, 5.4, 4.2, 6.45), 
    V4=c(0.166, 0.154, 0.177, 0.275, 0.298, 0.15, 0, 0.117, 0.155, 0.138, 0.249,
         0.143, 0.172, 0.178, 0.101, 0.008, 0.251, 0.366, 0.106, 0.089, 0.222, 
         0.187), 
    V5=c(0.1, 0.1, 2.9, 1.1, 1, 0.9, 2.4, 1.8, 2.3, 3.2, 3.5, 2.8, 2.5, 3, 3, 
         2.64, 2.7, 2.9, 2.9, 3.2, 2.9, 2),
    V6=c(0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 
         1L, 1L, 0L, 1L)), 
  .Names=c("V1", "V2", "V3", "V4", "V5", "V6"), 
  class="data.frame", 
  row.names = c(NA, -22L))
colnames(jet) <- c("FFD", "SPR", "RGF", "PLF", "SLF", "CAR")
rownames(jet) <- c(
  "FH-1", "FJ-1", "F-86A", "F9F-2", "F-94A", "F3D-1", "F-89A", "XF10F-1",
  "F9F-6", "F-100A", "F4D-1", "F11F-1", "F-101A", "F3H-2", "F-102A", "F-8A", 
  "F-104B", "F-105B", "YF-107A", "F-106A", "F-4B", "F-111A")
jet$CAR <- factor(jet$CAR, labels = c("no", "yes"))
X <- scale(jet[, c('SPR', 'RGF', 'PLF', 'SLF')], center=F, scale=T)
dj <- dist(X)
plot(cc <- hclust(dj), main='Jets')

pr <- prcomp(dj)$x[, 1:2]
plot(pr,
     pch=(1:2)[cutree(cc, k=2)],
     col=c('black', 'red')[jet$CAR],
     xlim=range(pr) * c(1, 1.5))
legend('topright',
       col=c(1, 1, 2, 2),
       legend=c('1:no', '2:no', '1:yes', '2:yes'),
       pch=c(1, 2, 1, 2),
       title='Cluster / CAR',
       bty='n')