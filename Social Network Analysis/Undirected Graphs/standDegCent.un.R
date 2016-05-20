standDegCent.un <- function(X) {
	#Calculates standard actor centrality
	#See p. 179 eq (5.3) in "Social Network Analysis" Wasserman & Faust
	if(dim(X)[1] != dim(X)[2]){
		print("Matrix must be square")
		break
		}
	if(identical(X, t(X))==F){
		print("Matrix is not undirected")
		break
		}
	out <- as.matrix(rowSums(X) / (dim(X)[1] - 1), nrow=dim(X)[1])
	colnames(out) <- "Actor Standard Degree Centality"
	out
	}