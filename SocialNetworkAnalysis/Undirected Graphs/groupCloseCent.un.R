groupCloseCent.un <- function(D) {
	#Calculates Freeman's general group closeness index
	#See p. 186, eq (5.9) in "Social Network Analysis: Methods and Applications" by Wasserman & Faust
	#NOTE: Takes a distance matrix as input
	
	##SOMETHING WRONG: should have output range [0, 1], but Florentine matrix -> 1.302
	
	if(dim(D)[1] != dim(D)[2]){
		print("Matrix must be square")
		break
		}
	if(identical(D, t(D))==F){
		print("Matrix is not undirected")
		break
		}
	g <- dim(D)[1]
	CCnMax <- 1
	
	CCn <- (g-1) / (rowSums(D) - 2)
	CC <- sum(CCnMax - CCn) / (((g-1)*(g-2)) / (2*g - 3))
	CC
	}