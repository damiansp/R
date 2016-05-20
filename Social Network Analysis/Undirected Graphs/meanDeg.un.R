meanDeg.un <- function(X) {
	#Calculates mean degrees in network
	#See p. 180, eq. (5.6) in "Social Network Analysis: Methods and Applications" by Wasserman & Faust
	if(dim(X)[1] != dim(X)[2]){
		print("Matrix must be square")
		break
		}
	if(identical(X, t(X))==F){
		print("Matrix is not undirected")
		break
		}
	VarD <- var(rowSums(X)) #variance of connectedness
	mean(rowSums(X))
	}