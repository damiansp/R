#=========================#
#						  #
#  Graphic Models with R  #
#						  #
#=========================#
rm(list=ls())

library(graph)
library(RBGL)
library(gRbase)
library(Rgraphviz)
library(ggm)

load('~/Desktop/R/GraphicalModels/GM.RData')
# Chapter 1: Graphs and Conditional Independence

# 1.2 Graphs
	# 1.2.1 Undirected Graphs
	ug0 <- ug(~a:b, ~b:c:d, ~e)
	# All of the following are equivalent assignments:
	# ug0 <- ug(~a:b+b:c:d+e)
	# ug0 <- ug(~a*b+b*c*d+e)
	# ug0 <- ug(c('a','b'), c('b','c','d'), 'e')
	ug0
	plot(ug0)
	
	# ug returns a graphNEL object, but may be changed by:
	# result='igraph', or result='matrix' parameters
	ug0i <- ug(~a:b+b:c:d+e, result='igraph')
	ug0i
	plot(ug0i, layout=layout.spring)
	
	myiplot <- function(x, ...) {
		V(x)$size <- 30				# NO SUCH FUNCTION V()!
		V(x)$label.cex <- 3
		plot(x, ...)
	}
	
	myiplot(ug0i)
	
	# Adding and removing edges:
	ug0a <- addEdge('a', 'c', ug0)
	plot(ug0a)
	ug0a <- removeEdge('c','d', ug0)
	plot(ug0a)
	ug0 <- ug(~a:b, ~b:c:d, ~e)
	
	# To retrieve nodes, edges
	nodes(ug0)
	edges(ug0)
	edgeList(ug0)
	edgeList(ug0, matrix=T)
	str(edgeList(ug0))
	
	# Checking cliques and subsets
	is.complete(ug0)
	is.complete(ug0, set=c('b','c','d'))
	maxClique(ug0)
	
	separates('a','d', c('b', 'c'), ug0)
	separates('a','d', c('b'), ug0) 		# b separates a from d
	
	# Subgraphs
	ug1 <- subGraph(c('b','c','d','e'), ug0)
	plot(ug1)
	# The boundary of a, bd(a), is the set of all nodes adjacent to a
	# The closure of a, cl(a) = union(bd(a), a)
	adj(ug0, 'c')
	closure('c', ug0)
	
	# 1.2.2 Directed Acyclic Graphs (DAG)
	# Create DAGs with dag()
	# All the following are equivalent:
	dag0 <- dag(~a, ~b*a, ~c*a*b, ~d*c*e, ~e*a, ~g*f)	# ":" can be used in place of "*"
	plot(dag0)
	dag0 <- dag(~a + b*a + c*a*b + d*c*e + e*a + g*f)
	dag0 <- dag(~a + b|a + c|a*b + d|c*e + e|a + g|f)
	dag0 <- dag('a', c('b', 'a'), c('c', 'a', 'b'), c('d', 'c', 'e'), c('e','a'), c('g', 'f'))
	dag0
	# dag can return igraph or matrix instead of default graphNEL with result='igraph' or 
	# result='matrix'
	# To retrieve nodes:
	nodes(dag0)
	# Edges
	edges(dag0) # a list [[nodes]][children]
	edgeList(dag0)	# parent child
	vpar(dag0) # a list [[nodes]][self parents]
	parents('d', dag0)
	children('c', dag0)
	ancestralSet(c('b', 'e'), dag0)
	plot(ancestralGraph(c('b','c','e'), dag0))
	# moralize
	dag0m <- moralize(dag0)
	plot(dag0m)
	
	# 1.2.3 Mixed Graphs
	adjm <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,0, 1,1,1,0), nrow=4)
	rownames(adjm) <- colnames(adjm) <- letters[1:4]
	adjm
	plot(adjm)
	gG <- as(adjm, 'graphNEL')
	plot(gG)
	plot(gG, 'neato')
	# NOTE: some classes for graphs don't distinguish between undirected and bidirected edges:
	gG1 <- as(adjm, 'igraph')
	d1 <- matrix(0,11,11)
	d1[1,2] <- d1[2, 1] <- d1[1, 3] <- d1[3, 1] <- d1[2, 4] <- d1[4, 2] <- d1[5, 6] <- 
		d1[6, 5] <- 1
	d1[9, 10] <- d1[10, 9] <- d1[7, 8] <- d1[8, 7] <- d1[3, 5] <- d1[5, 10] <- d1[4, 6] <-
		d1[4, 7] <- 1
	d1[6, 11] <- d1[7, 11] <- 1
	rownames(d1) <- colnames(d1) <- letters[1:11]
	cG1 <- as(d1, 'graphNEL')
	
# 1.3 Conditional Independence of Graphs
separates('a', 'd', 'b', ug0) # b seps a from d?	# NOTE: separation implies conditional
													# independence
# To see if two variables are marginally independent, ask if sep'd by the empty set:
separates('a', 'd', character(0), ug0)
# In DAGs, d-separated nodes are condtionally indepenent
d.separates <- function(a, b, c, dag) {
	separates(a, b, c, gRbase::moralize(ancestralGraph(union(union(a, b), c), dag)))
}

d.separates('c', 'e', 'a', dag0)

# The ggm package also has a function dSep()
dSep(as(dag0, 'matrix'), 'c', 'e', 'a')



# 1.4 More About Graphs
	# 1.4.1 Special Properties
	plot(ug0)
	is.simplicial('b', ug0) # a node is 'simplicial' if its boundary is complete (??)
	simplicialNodes(ug0)
	connComp(ug0)	# Connected components
	is.triangulated(ug0)
	is.decomposition('a', 'd', c('b', 'c'), ug0)	# d is complete and separates {a} from														# {b, c}, but V != {a, b, c, d}
	


save.image('~/Desktop/R/GraphicalModels/GM.RData')