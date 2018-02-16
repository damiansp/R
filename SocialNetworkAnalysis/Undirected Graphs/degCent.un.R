degCent.un <- function(X) {
	#Calculates group degree centrality in network
	#See p. 180, eq. (5.5) in "Social Network Analysis: Methods and Applications" by Wasserman & Faust
	if(dim(X)[1] != dim(X)[2]){
		print("Matrix must be square")
		break
		}
	if(identical(X, t(X))==F){
		print("Matrix is not undirected")
		break
		}
	nMax <- max(rowSums(X))
	sum(nMax - rowSums(X)) / ((dim(X)[1]-1) * (dim(X)[1]-2))
	}