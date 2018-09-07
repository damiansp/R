#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')

library(ggm)
library(gRbase)
library(gRim)
library(igraph)
library(lcd)
library(RBGL)
library(Rgraphviz)
library(sna)
data(dumping)
data(lizard)
data(lizardAGG)
data(lizardRAW)
data(mildew)
data(reinis)



# 2. Preliminaries
# 2.1 Four data sets
str(reinis)
str(mildew)
str(dumping)


# 2.2 Data Formats
head(lizardRAW)
lizardAGG
lizard



# 3. Log-Linear Models
# 3.2 Hierarchical log-linear models
m1 <- dmod(~species*height + species*diam, data=lizard) # same as
m2 <- dmod(list(c('species', 'height'), c('species', 'diam')), lizard)
