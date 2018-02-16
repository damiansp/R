smoother <- function(x){
	size <- length(x)
	mat <- matrix(0, size, size)
	mat[1,1] <- 0.5
	for(i in 2:size){
		mat[i, i-1] <- mat[i,i] <- 0.5
		}
	mat
	}