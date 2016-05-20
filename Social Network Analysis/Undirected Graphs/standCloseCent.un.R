standCloseCent.un <- function(D) {
	#Calculates standardized actor closeness centrality
	#See p. 184, eq. (5.7) in "Social Network Analysis: Methods and Applications" by Wasserman & Faust
	#NOTE: Takes a distance matrix as input
	if(dim(D)[1] != dim(D)[2]){
		print("Matrix must be square")
		break
		}
	if(identical(D, t(D))==F){
		print("Matrix is not undirected")
		break
		}
	CC <- as.matrix(1/(rowSums(D) - 2), ncol=1) # -2 removes path to self
	C.C <- (dim(D)[1] - 1) * CC
	colnames(C.C) <- "St. Actor Closeness Centrality"
	C.C
	}