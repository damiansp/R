distMatrix.un <- function(X) {
	#Takes a sociomatrix X, and returns the distance matrix D
	if(dim(X)[1] != dim(X)[2]){
		print("Matrix must be square")
		break
		}
	if(identical(X, t(X))==F){
		print("Matrix is not undirected")
		break
		}
	
	m <- dim(X)[1] #dimension of matrix
	D <- X	#initialize dist matrix
	for(i in 2:(m-1)){
		D.aug <- D %*% X
		for(j in 1:m){
			for(k in 1:m){
				if(D[j, k]==0 && D.aug[j, k]!=0)
					D[j, k] <- i
				}
			}
		}
		
	for(i in 1:m){
		for(j in 1:m){
			if(D[i,j]==0)
				D[i,j] <- Inf
			}
		}
	D		
	}