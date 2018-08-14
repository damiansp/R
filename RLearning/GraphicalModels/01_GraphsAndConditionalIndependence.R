#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')
#source('https://bioconductor.org/biocLite.R')
#biocLite('lcd')
#biocLite('RBGL')
#biocLite('Rgraphviz')

library(ggm)
library(gRbase)
library(igraph)
library(lcd)
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

dag0m <- moralize(dag0)
plot(dag0m)

# 2.3 Mixed Graphs
adj.m <- matrix(c(0, 1, 1, 0,
                  1, 0, 0, 1,
                  1, 0, 0, 0,
                  1, 1, 1, 0),
                nrow=4)
rownames(adj.m) <- colnames(adj.m) <- letters[1:4]
adj.m
gG <- as(adj.m, 'graphNEL')
plot(gG)
plot(gG, 'neato')
#graphvizCapabilities()$layoutTypes
plot(gG, 'dot') # default
plot(gG, 'circo')
plot(gG, 'fdp')
plot(gG, 'osage')
plot(gG, 'twopi')

gG1 <- as(adj.m, 'igraph')
plot(gG1)
plot(gG1, layout=layout.spring)
#  bidirectionals as segments instead of arrows:
E(gG1)$arrow.mode <- c(2, 0)[1 + is.mutual(gG1)]
plot(gG1)

?plot.igraph

d1 <- matrix(0, 11, 11)
d1[1, 2] <- d1[2, 1] <- d1[1, 3] <- d1[3, 1] <- d1[2, 4] <- d1[4, 2] <- 1
d1[5, 6] <- d1[6, 5] <- d1[9, 10] <- d1[10, 9] <- d1[7, 8] <- d1[8, 7] <- 1
d1[3, 5] <- d1[5, 10] <- d1[4, 6] <- d1[4, 7] <- d1[6, 11] <- d1[7, 11] <- 1
rownames(d1) <- colnames(d1) <- letters[1:11]
cG1 <- as(d1, 'igraph')
E(cG1)$arrow.mode <- c(2, 0)[1 + is.mutual(cG1)]
plot(cG1, layout=layout.spring)

is.chaingraph(as(cG1, 'matrix'))
cGm <- as(moralize(dag(adj.m)), 'graphNEL')
plot(cGm)



# 3. Conditional Independence and Graphs
plot(ug0)
separates('a', 'd', 'b', ug0) # {b} separates a from d in ug0: T
# hence a cond.indep. of d given b
# Marginally independent? (= separated by the empty set?)
separates('a', 'd', character(0), ug0) # F
separates('a', 'e', character(0), ug0) # T

d.separates <- function(a, b, c, dag) {
  separates(a, 
            b, 
            c, 
            gRbase::moralize(ancestralGraph(union(union(a, b), c), dag)))
}

plot(dag0)
d.separates('c', 'e', 'a', dag0) # T

# from library(ggm)
dSep(as(dag0, 'matrix'), 'c', 'e', 'a') # T
# library(lcd) (not available)
#is.separated('e', 'g', c('k'), as(cG1, 'matrix')) # F: e not cond. ind of g | k



# 4. More About Graphs
# 4.1 Special properties
is.simplicial('b', ug0) # F
simplicialNodes(ug0) # a, c, d, e
connComp(ug0) # {a, b, c, d}, {e}
is.triangulated(ug0) # T (?)
is.decomposition(set='a', set2='d', set3=c('b', 'c'), ug0) # F
mcs(ug0) # maximum cardinality search: a, b, c, d, e
mcs(ug0, root=c('d', 'c', 'a')) # d, c, b, a, e
rip(ug0) # running intersection property: cliques {b, a}, {b, c, d}, {e}
         # separators                              -       b          -
         # parents                                 0       1          0
ug2 <- ug(~a:b:c + c:d + d:e + a:e)
plot(ug2)
is.triangulated(ug2) # F
ug3 <- triangulate(ug2)
is.triangulated(ug3) # T
plot(ug3)

G1 <- ug(~a:b + b:c + c:d + d:e + e:f + a:f + b:e)
mt1.G1 <- minimalTriang(G1)
G2 <- ug(~a:b:e:f + b:c:d:e)
mt2.G1 <- minimalTriang(G1, tobject=G2)
par(mfrow=c(2, 2))
plot(G1, main='G1')
plot(mt1.G1)
plot(G2, main='G2')
plot(mt2.G1)

par(mfrow=c(1, 3))
G1 <- ug(~a:b + b:c + c:d + d:e + e:f + a:f + b:e)
G1.rip <- mpd(G1)
G1.rip
plot(G1)
plot(subGraph(G1.rip$clique[[1]], G1))
plot(subGraph(G1.rip$clique[[2]], G1))

par(mfrow=c(1, 1))
plot(dag0)
adj(moralize(dag0), 'e')


# 4.2 Layout in Rgraphviz
plot(dag0, attrs=list(node=list(fillcolor='lightgrey', fontcolor='red')))
edgeNames(ug3)
ng3 <- agopen(ug3, name='ug3', layoutType='fdp')
ng4 <- ng3
AgEdge(ng4) <- AgEdge(ng4)[-3]
par(mfrow=c(1, 2))
plot(ng3)
plot(ng4)
par(mfrow=c(1, 1))
