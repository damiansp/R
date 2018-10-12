#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/GraphicalModels')
#source('https://bioconductor.org/biocLite.R')

#library(ggm)
library(gRain)
library(gRbase)
library(gRim)
#library(igraph)
#library(lcd) # not installable
#library(RBGL)
#library(Rgraphviz)
#library(sna)
data(chestSim500)
data(reinis)

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
yn <- c('yes', 'no')
a <- cptable(~asia, values=c(1, 99), levels=yn)
t.a <- cptable(~tub|asia, values=c(5, 95, 1, 99), levels=yn)
s <- cptable(~smoke, values=c(5, 5), levels=yn)
l.s <- cptable(~lung|smoke, values=c(1, 9, 1, 99), levels=yn)
b.s <- cptable(~bronc|smoke, values=c(6, 4, 3, 7), levels=yn)
e.lt <- cptable(~either|lung|tub, values=c(1, 0, 1, 0, 1, 0, 0, 1), levels=yn)
x.e <- cptable(~xray|either, values=c(98, 2, 5, 95), levels=yn)
d.be <- cptable(~dysp|bronc|either, values=c(9, 1, 7, 3, 8, 2, 1, 9), levels=yn)


# 2.2 Building the network
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
grn1 <- grain(plist)
summary(grn1)
plot(grn1)

# 2.2.1 Compilation - finding the clique potentials
grn1c <- compile(grn1)
summary(grn1c)

g <- grn1$dag
mg <- moralize(g)
tmg <- triangulate(mg)
rip(tmg)
plot(grn1c, type='jt')

# 2.2.2 Propagation - from clique potentials to clique marginals
grn1c <- propagate(grn1c)
summary(grn1c)


# 2.3 Absorbing evidence and answering queries
grn1c.ev <- setFinding(grn1c, nodes=c('asia', 'dysp'), states=c('yes', 'yes'))
querygrain(grn1c.ev, nodes=c('lung', 'bronc'), type='marginal')
querygrain(grn1c, nodes=c('lung', 'bronc'), type='marginal')
getFinding(grn1c.ev)
pFinding(grn1c.ev)

querygrain(grn1c.ev, nodes=c('lung', 'bronc'), type='joint')
querygrain(grn1c.ev, nodes=c('lung', 'bronc'), type='conditional') # lung | bronc

grn1c2 <- compile(grn1, root=c('lung', 'bronc', 'tub'), propagate=T)
grn1c2.ev <- setFinding(grn1c2, nodes=c('asia', 'dysp'), states=c('yes', 'yes'))
system.time(
  for (i in 1:50) {
    querygrain(grn1c.ev, nodes=c('lung', 'bronc', 'tub'), type='joint')
  }) # 0.24  0.010  0.257

system.time(
  for (i in 1:50) {
    querygrain(grn1c2.ev, nodes=c('lung', 'bronc', 'tub'), type='joint')
  }) # 0.098  0.006  0.111
  
# Stepwise
grn1c.ev <- setFinding(grn1c, nodes='asia', states='yes', propagate=F)
grn1c.ev <- setFinding(grn1c, nodes='dysp', states='yes', propagate=F)
grn1c.ev <- propagate(grn1c.ev)

# Back pedal
grnc1.ev <- retractFinding(grn1c.ev, nodes='asia')
getFinding(grnc1.ev)



# 3. Further Topics
# 3.1 Building a network from data
sim.dag.chest <- grain(chest.dag, data=chestSim500)
sim.dag.chest <- compile(sim.dag.chest, propagate=T, smooth=0.1)
querygrain(sim.dag.chest, nodes=c('lung', 'bronc'), type='marginal')

# from undirected, triangulated graph
g <- list(~asia|tub, ~either|lung|tub, ~either|lung|smoke, ~bronc|either|smoke,
          ~bronc|dysp|either, ~either|xray)
my.ug <- ugList(g)
sim.ug.chest <- grain(my.ug, data=chestSim500)
sim.ug.chest <- compile(sim.ug.chest, propagate=T)
plot(sim.ug.chest)

head(reinis)
m0 <- dmod(~ .^., data=reinis)
plot(m0)
m1 <- stepwise(m0)
plot(m1)
reinis.grain <- grain(as(m1, 'graphNEL'), data=reinis)
plot(reinis.grain)
renis.grain <- compile(reinis.grain, propagate=T)
querygrain(reinis.grain, nodes=c('phys', 'protein'), type='marginal')
reinis.ev <- setFinding(
  reinis.grain, nodes=c('systol', 'smoke', 'mental'), states=c('y', 'y', 'y'))
querygrain(reinis.ev, nodes=c('phys', 'protein'), type='marginal')


# 3.2 Bayesian networks with RHugin (omitted)


# 3.3 Simulation