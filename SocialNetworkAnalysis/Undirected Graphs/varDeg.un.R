varDeg.un <- function(X) {
	#Calculates variance of degrees in network
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
	x <- rowSums(X)
	VarD.g <- sum((x - mean(x))^2) / length(x)
	list(paste("VarD:", VarD, sep=" "), paste("VarD.g:", VarD.g, sep=" "), "VarD is actual variance VarD.g divides by g instead of g + 1")
	}