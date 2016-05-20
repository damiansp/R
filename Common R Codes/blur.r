blur <- function(M, distance) {
	rows <- dim(M)[1]
	cols <- dim(M)[2]
	#Create empty matrix the size of M to write new values to
	N <- matrix(nrow=rows, ncol=cols)
	
	for (i in 1:rows) {
		#get range of possible row vals
		rowMin <- i - distance
		if (rowMin < 1) { rowMin <- 1 }
		rowMax <- i + distance
		if (rowMax > rows) { rowMax <- rows }

		for (j in 1:cols) {
			#get range of possible col vals
			colMin <- j - distance
			if (colMin < 1) { colMin <- 1 }
			colMax <- j + distance
			if (colMax > cols) { colMax <- cols}
			
			N[i, j] <- mean(M[rowMin:rowMax, colMin:colMax])
		}
	}
	N
}


medianFilter <- function(M, distance) {
	rows <- dim(M)[1]
	cols <- dim(M)[2]
	#Create empty matrix the size of M to write new values to
	N <- matrix(nrow=rows, ncol=cols)
	
	for (i in 1:rows) {
		#get range of possible row vals
		rowMin <- i - distance
		if (rowMin < 1) { rowMin <- 1 }
		rowMax <- i + distance
		if (rowMax > rows) { rowMax <- rows }

		for (j in 1:cols) {
			#get range of possible col vals
			colMin <- j - distance
			if (colMin < 1) { colMin <- 1 }
			colMax <- j + distance
			if (colMax > cols) { colMax <- cols}
			
			N[i, j] <- median(M[rowMin:rowMax, colMin:colMax])
		}
	}
	N
}