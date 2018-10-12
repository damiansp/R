#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)
data('USArrests')

df <- USArrests
df <- na.omit(df)
df <- scale(df)

head(df)