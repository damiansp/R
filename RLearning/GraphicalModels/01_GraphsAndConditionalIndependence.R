#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')
#source('https://bioconductor.org/biocLite.R')
#biocLite('RBGL')

library(gRbase)


# 2. Graphs
# 2.1 Undirected graphs
ug0 <- ug(~a:b, ~b:c:d, ~e)