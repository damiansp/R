#===================================#
#									#
#	Generalized Additive Models		#
#		An Introduction with R		#
#									#
#	Simon N. Wood. 2006				#
#									#
#===================================#

#===========================#
#							#
#	4. Some GAM Theory		#
#							#
#===========================#
rm(list = ls())
#install.packages('gamair', repos = 'http://cran.us.r-project.org')
library(gamair)
load('~/Desktop/R/GAM/GAM.RData')

# 4.1 Smoothing Bases
  # 4.1.4 P-splines
  # Evaluate the ith b-spline basis function of order m at the values x, given knots
  # at k
  bspline <- function(x, k, i, m = 2) {
  	if (m == -1) { # base of recursion
  	  res <- as.numeric(x < k[i + 1] & x >= k[i])
  	} else {
  	  # Construct from call to lower-order basis
  	  z0 <- (x - k[i]) / (k[i + m + 1] - k[i])
  	  z1 <- (k[i + m + 2] - x) / (k[i + m + 2] - k[i + 1])
  	  res <- z0 * bspline(x, k, i, m - 1) + z1 * bspline(x, k, i + 1, m - 1)
  	}
  	
  	return (res)
  }

  k <- 6
  P <- diff(diag(k), differences = 1)
  S <- t(P) %*% P	# penalty matrix
  
  


save.image('~/Desktop/R/GAM/GAM.RData')