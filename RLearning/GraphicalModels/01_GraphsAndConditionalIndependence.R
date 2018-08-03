#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')
#source('https://bioconductor.org/biocLite.R')
#biocLite('RBGL')
#biocLite('Rgraphviz')

library(gRbase)
library(igraph)
library(RBGL)
library(Rgraphviz)


# 2. Graphs
# 2.1 Undirected graphs
ug0 <- ug(~a:b, ~b:c:d, ~e) # same as
ug0 <- ug(~a:b + b:c:d + e) # same as
ug0 <- ug(~a*b + b*c*d + e) # same as
ug0 <- ug(c('a', 'b'), c('b', 'c', 'd'), 'e')
plot(ug0)
ug0

ug0i <- ug(~a:b + b:c:d + e, result='igraph')
ug0i

ug0m <- ug(~a:b + b:c:d + e, result='matrix')
ug0m

#plot(ug0i, layout=layout.spring)
layout.spring(ug0i)

ug0a <- addEdge('a', 'c', ug0)
ug0a <- removeEdge('c', 'd', ug0)
plot(ug0a)

nodes(ug0)
edges(ug0)
edgeList(ug0)
is.complete(ug0)
is.complete(ug0, c('b', 'c', 'd'))
maxClique(ug0)
separates('a', 'd', c('b', 'c'), ug0)
ug1 <- subGraph(c('b', 'c', 'd', 'e'), ug0)
plot(ug1)

adj(ug0, 'c') # adjacent/neighors
closure('c', ug0) # adj + self

# 2.2 Directed acyclic graphs
