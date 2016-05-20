#===================================================================================#
#																					#
#	Using a set of paired words as input, create a matrix of letter coincidences	#
#																					#
#===================================================================================#
source('~/Desktop/R/Common R Codes/allison.binoms.R', chdir = TRUE)
source('~/Desktop/R/Common R Codes/g.test.R', chdir = TRUE)


d <- read.csv('~/Desktop/R/linguistics/enSpaTest.csv', stringsAsFactors=F)
l1chars <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 
			 'q', 'r', 's', 't' , 'u', 'v', 'w', 'x', 'y', 'z')
l2chars <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'l', 'm', 'n', 'ñ', 'o', 'p', 
			 'q', 'r', 's', 't' , 'u', 'v', 'x', 'y', 'z')

# data should be a list of paired words
makeCoincMatrix <- function(data) {
	# Create a placeholder matrix to hold actual correspondences of letters, init at 0s
	coincM <- matrix(data= 0, nrow=length(l1chars), ncol=length(l2chars), 
					 dimnames=list(l1chars, l2chars))
	n <- dim(data)[1]

	# loop through all words in the list
	for (i in 1:n) {
		w1 <- strsplit(data[i, 1], '')
		w2 <- strsplit(data[i, 2], '')
	
		# loop through the letters of the w1
		for (ch in w1[[1]]) {
			# loop through the letters of w2, updating all matches in coincM
			for (chr in w2[[1]]) {
				coincM[ch, chr] <- coincM[ch, chr] + 1
			}
		}

	}

	return (coincM)
}


# Randomly permute l2, and repeat the process ITERS times, but write to randM instead
makeRandMatrix <- function(data, iters=1000) {
	randM <- matrix(data= 0, nrow=length(l1chars), ncol=length(l2chars), 
					dimnames=list(l1chars, l2chars))

	for (i in 1:iters) {
		n <- dim(data)[1]
		l2 <- sample(data[, 2])
	
		for (i in 1:n) {
			w1 <- strsplit(data[i, 1], '')
			w2 <- strsplit(l2[i], '')
	
			# loop through the letters of the w1
			for (ch in w1[[1]]) {
				# loop through the letters of w2, updating all matches in coincM
				for (chr in w2[[1]]) {
					randM[ch, chr] <- randM[ch, chr] + 1
				}
			}
		}
		
	}
	
	return (randM)
}