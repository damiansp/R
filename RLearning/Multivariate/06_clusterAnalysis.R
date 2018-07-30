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