#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(MVA)

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
