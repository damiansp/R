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
ug0 <- ug(~a:b + b:c:d + e) # same as ***
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
dag0 <- dag(~a, ~b*a, ~c*a*b, ~d*c*e, ~e*a, ~g*f) # ~b*a: a -> b # same as:
dag0 <- dag(~a + b*a + c*a*b + d*c*e + e*a + g*f) # same as:
dag0 <- dag(~a + b|a + c|a*b + d|c*e + e|a + g|f) # same as: ***
dag0 <- dag('a', c('b', 'a'), c('c', 'a', 'b'), c('d', 'c', 'e'), c('e', 'a'), 
            c('g', 'f'))
plot(dag0)

nodes(dag0)
str(edges(dag0))
str(edgeList(dag0))
vpardag0 <- vpar(dag0)
vpardag0
parents('d', dag0)
children('a', dag0)
ancestralSet(c('b', 'e'), dag0)
plot(ancestralGraph(c('b', 'e'), dag0))