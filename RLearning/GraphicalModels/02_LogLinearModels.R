#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')

library(ggm)
library(gRbase)
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
