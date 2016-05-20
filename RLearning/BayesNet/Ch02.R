#========================================#
#										 #
#  Bayesian Networks in R                #
#  Nagarajan, Scutari, and Lèbre (2013)  #
#										 #
#========================================#

# 1 Introduction
source('http://bioconductor.org/biocLite.R')
biocLite('Rgraphviz')
biocLite('graph')
biocLite('RBGL')
library(bnlearn)
library(deal)	# NOTE: this package masks 'modelstring, 'nodes', and 'score' from bnlearn
library(catnet)
library(pcalg)	# Masks 'dag2cpdag' from catnet & 'ci.test', 'dsep', 'pdag2dag', 'shd', 
				# 'skeleton' from bnlearn
data(lizards)


# 2 Bayesian Networks in the Absence of Temporal Information
	# 2.1 Bayesian Networks: Essential Definitions and Properties
		# 2.1.1 Graph Structure and Probability Factorization
		# 2.1.2 Fundamental Connections
		# 2.1.3 Equivalent Structures
		# 2.1.4 Markov Blankets
	
	# 2.2 Static Bayesian Network Modeling
		# 2.2.1 Constraint-Based Structure-Learning Algorithms
		# 2.2.2 Score-Based Structure-Learning Algorithm
		# 2.2.3 Hybrid Structure-Learning Algorithms
		# 2.2.4 Choosing Distributions, Conditional Independence Tests, and Network Scores
		
	# 2.3 Static Bayesian Network Modeling in R
		# 2.3.1 Popular R Packages for Bayesian Network Modeling
		# { bnlearn, catnet, deal, pcalg, gRbase, gRain }
		# 2.3.2 Creating and Manipulating Network Structures
		data(marks)
		str(marks)
		ug <- empty.graph(names(marks))
		plot(ug)
		arcs(ug, ignore.cycles=T) <- matrix(c('MECH','VECT', 'MECH','ALG', 'VECT', 'MECH',
											  'VECT','ALG', 'ALG','MECH', 'ALG','VECT', 
											  'ALG','ANL', 'ALG','STAT', 'ANL','ALG',
											  'ANL','STAT', 'STAT','ALG', 'STAT','ANL'), 
											ncol=2, byrow=T, 
											dimnames=list(c(), c('from', 'to')))
		plot(ug)
		
		dag <- empty.graph(names(marks))
		arcs(dag) <- matrix(c('VECT','MECH', 'ALG','MECH', 'ALG','VECT', 'ANL','ALG', 
							  'STAT','ALG', 'STAT','ANL'), ncol=2, byrow=T, 
							dimnames=list(c(), c('from', 'to')))
		plot(dag)
		
		# can also construct via an adjacency matrix:
		mat <- matrix(c(0,1,1,0,0, 0,0,1,0,0, 0,0,0,1,1, 0,0,0,0,1, 0,0,0,0,0), nrow=5, 
					  dimnames=list(bnlearn::nodes(dag), bnlearn::nodes(dag)))
		dag2 <- empty.graph(bnlearn::nodes(dag))
		amat(dag2) <- mat
		plot(dag2)
		all.equal(dag, dag2)
		
		# can also construct by edits to an existing network
		dag3 <- empty.graph(bnlearn::nodes(dag))
		dag3 <- set.arc(dag3, 'VECT', 'MECH')
		plot(dag3)
		dag3 <- set.arc(dag3, 'ALG', 'MECH')
		dag3 <- set.arc(dag3, 'ALG', 'VECT')
		dag3 <- set.arc(dag3, 'ANL', 'ALG')
		dag3 <- set.arc(dag3, 'STAT', 'ALG')
		dag3 <- set.arc(dag3, 'STAT', 'ANL')
		# also available: { rev.arc, drop.arc }
		all.equal(dag, dag3)
		all.equal(ug, moral(dag))
		
		# topological ordering of the network
		node.ordering(dag)
		# neighborhood
		nbr(dag, 'ANL')
		# Markov blanket
		nbr(dag, 'STAT')
		chld <- children(dag, 'VECT')
		par <- parents(dag, 'VECT')
		# other parents of children
		o.par <- sapply(chld, parents, x=dag)
		o.par <- o.par[o.par != 'VECT']
		sort(unique(c(chld, par, o.par))) == sort(mb(dag, 'VECT'))
		bnlearn::score(dag, data=marks, type='loglik-g')
		vstructs(dag)
		vstructs(dag3)
		vstructs(dag, moral=T)
		
		# Equivalence class or Completed Partially Directed Acyclic Graph
		plot(cpdag(dag3))
		
		# Using the deal package
		deal.net = network(marks)
		deal.net
		m <- '[MECH][VECT|MECH][ALG|MECH:VECT][ANL|ALG][STAT|ALG:ANL]'
		deal.net <- as.network(m, deal.net)
		plot(deal.net)
		deal.net
		
		# Using the catnet package
		cat.net <- cnCatnetFromEdges(names(marks), 
									 list(MECH=NULL, VECT='MECH', ALG=c('MECH', 'VECT'), 
									 	  ANL='ALG', STAT=c('ALG', 'ANL')))
		cat.net
		chld <- cnEdges(cat.net)$VECT 	# children of VECT
		par <- cnParents(cat.net)$VECT
		o.par <- sapply(chld, function(node) { cnEdges(cat.net)[[node]] } )
		unique(unlist(c(chld, par, o.par[o.par != 'VECT'])))
		em <- empty.graph(names(marks))
		plot(em)
		arcs(em) <- cnMatEdges(cat.net)
		em <- model2network(deal::modelstring(deal.net))  	# This indicates the modelstring 
															# function from the deal package
															# (instead of from bnlearn if 
															# masking)
		# 2.3.3 Plotting Network Structures
		hl2 <- list(arcs=vstructs(dag2, arcs=T), lwd=4, col='black')
		hl3 <- list(arcs=vstructs(dag3, arcs=T), lwd=4, col='blue')
		graphviz.plot(dag2, highlight=hl2, layout='fdp', main='DAG2')
		graphviz.plot(dag3, highlight=hl3, layout='fdp', main='DAG3')
		graphviz.plot(cpdag(dag2), highlight=hl2, layout='fdp', main='cpdag(dag2)')
		graphviz.plot(cpdag(dag3), highlight=hl3, layout='fdp', main='cpdag(dag3)')
		
		# 2.3.4 Structural Learning
		bn.gs <- gs(marks)	# results in a different graph than implemented in the book
		all.equal(bn.gs, iamb(marks))
		all.equal(bn.gs, inter.iamb(marks))
		all.equal(bn.gs, iamb(marks, test='mc-cor'))
		plot(bn.gs)
		suff.stat <- list(C=cor(marks), n=nrow(marks))
		pc.fit <- pc(suff.stat, indepTest=gaussCItest, p=ncol(marks), alph=0.05)
		pc.fit
		plot(pc.fit)
		
		gs.graph <- as.graphAM(bn.gs)
		plot(gs.graph)
		compareGraphs(pc.fit@graph, gs.graph)	# tpr: true positive rate
												# tdr: true discovery rate
												# a value of 1 indicates that the proportion in
												# pc.fit identified with respect to gs.graph is
												# 1 (i.e., the network structures are
												# identical)
		bn.hc <- hc(marks) # hill-climbing algo
		bn.hc
		plot(bn.hc)
		# This yields a diff network, but scores are equal
		bnlearn::score(bn.gs, data=marks, type='bic-g') # Error (see line 129 above)
		bnlearn::score(bn.hc, data=marks, type='bic-g')
		
		# Learning structure with deal package
		net <- network(marks)
		prior <- jointprior(net, N=5) # doesn't work bc jointprior calls jointdisc which 
									  # expects net to have a $discrete attribute, which it 
									  # does not, and for net$nodes[[i]] to have attribute
									  # $levels, which it does not.
		
									  
		# 2.3.5 Parameter Learning
		fitted <- bn.fit(bn.gs, data=marks)	# Doesn't work bc bn.gs is not directed
		fitted$ALG
		fitted <- bn.fit(bn.hc, data=marks)
		fitted$ALG
		fitted$ALG <- list(coef=c('(Intercept)'=25, 'MECH'=0.5, 'VECT'=0.25), sd=8.1)
		fitted$ALG
		fitted$MECH
		fitted$VECT
		VECT.par <- list(coef=c('(Intercept)'=35, 'MECH'=0.4, sd=11))
		dist <- list(MECH=fitted$MECH, VECT=VECT.par, ALG=fitted$ALG, ANL=fitted$ANL, 
					 STAT=fitted$STAT)
		fitted2 <- custom.fit(bn.hc, dist=dist)
		
		# 2.3.6 Discretization
		dmarks <- discretize(marks, breaks=2, method='interval')
		bn.dgs <- gs(dmarks)
		bn.dhc <- hc(dmarks)
		all.equal(cpdag(bn.dgs), cpdag(bn.dhc))
		plot(bn.dgs)
		plot(bn.dhc)
		fitted <- bn.fit(bn.dhc, data=dmarks)
		fitted$ALG
		netlist <- cnSearchSA(dmarks)
		best <- cnFindBIC(netlist, nrow(dmarks))
		cnMatEdges(best)
		