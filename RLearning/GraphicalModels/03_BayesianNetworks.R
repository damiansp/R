#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')
#source('https://bioconductor.org/biocLite.R')

#library(ggm)
library(gRain)
#library(gRbase)
#library(gRim)
#library(igraph)
#library(lcd) # not installable
#library(RBGL)
#library(Rgraphviz)
#library(sna)

#library(RHugin) # No 3.5 implementation
# Availble here: http://rhugin.r-forge.r-project.org/
# but requires installation of (commercial) HUGIN software



# 1. Intro
# 1.1 Chest clinic example
# 1.2 Models Base on Directed Acyclic Graphs
g <- list(~asia, ~tub|asia, ~smoke, ~lung|smoke, ~bronc|smoke, ~either|lung:tub,
          ~xray|either, ~dysp|bronc:either)
chest.dag <- dagList(g)
plot(chest.dag)

#d.separates('tub', 'smoke', c('dysp', 'xray'), chest.dag) # F
#d.separates('tub', 'lung', 'smoke', chest.dag) # T



# 2 Building and Using Bayesian Networks
# 2.1 Specification of conditional probability tables

