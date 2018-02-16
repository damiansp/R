CA <- function(width=100, iter=500, rules=NULL, init=NULL, caps=NULL) {
	# Initialize matrix row 1
	M <- matrix(nrow=iter, ncol=width)
	if (is.null(init)) { 
		M[1, ] <- sample(c(0,1), size=width, T)
	} else { M[1, ] <- init }
	
	# Create random rules
	if (is.null(rules)) {
		rules <- sample(c(0, 1), size=8, T)
	}
	
	# Iterate over rows 
	for (r in 2:iter) {
		# and cols:
		for (clm in 2:(width - 1)) {
			input <- c(M[(r-1), (clm-1):(clm+1)])
			
			# Possible rules
			if (sum(input == c(0, 0, 0)) == 0) { M[r, clm] <- rules[1] }
			if (sum(input == c(0, 0, 1)) == 0) { M[r, clm] <- rules[2] }
			if (sum(input == c(0, 1, 0)) == 0) { M[r, clm] <- rules[3] }
			if (sum(input == c(0, 1, 1)) == 0) { M[r, clm] <- rules[4] }
			if (sum(input == c(1, 0, 0)) == 0) { M[r, clm] <- rules[5] }
			if (sum(input == c(1, 0, 1)) == 0) { M[r, clm] <- rules[6] }
			if (sum(input == c(1, 1, 0)) == 0) { M[r, clm] <- rules[7] }
			if (sum(input == c(1, 1, 1)) == 0) { M[r, clm] <- rules[8] }
		}
		
		# Caps
		if (is.null(caps)) {
			M[r, c(1, width)] <- sample(c(0, 1), 2, T)
		} else { 
			M[r, 1] <- caps[1]
			M[r, width] <- caps[2]
		}
	}
	
	par(mar=c(0, 0, 0, 0))
	image(M)
	
}