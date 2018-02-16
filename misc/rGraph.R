#-------------------------------------------------------------------------------#
#                                                                               #
#  Makes and plots random networks with probability of any two nodes specified  #
#  @author Damian Satterthwaite-Phillips <damiansp@damianphillps.com>           #
#  @version 26Sep2013                                                           #
#                                                                               #
#-------------------------------------------------------------------------------#

library(MASS)

# Inputs n = number of nodes; p = probability of any 2 nodes being connected
rGraph <- function(n, p, graph=T) {
	# locate nodes in space (randomly distributed)
	lociX <- runif(n)
	lociY <- runif(n)
	
	if (graph == T) {
		par(mfrow=c(2,1))
		plot(lociX, lociY)
	}
	
	# Construct a connection matrix
	m <- matrix(sample(c(1, 0), n*n, T, c(p, 1-p)), n, n)
	
	# Main diagonal should be all 0
	d <- 1*(!(diag(n)))
	m <- m*d
	
	# Make symmetrical
	for (i in 2:n) {
		for (j in 1:(i - 1)) {
			m[j, i] <- m[i, j]
		}
	}
	
	if (graph == T) {	
		# Draw connections on graph
		# Loop through rows
		for (i in 2:n) {
			# And cols
			for (j in 1:(i - 1)) {
				if (m[i, j] == 1) {
					lines(c(lociX[i], lociX[j]), c(lociY[i], lociY[j]))	
				}
			}
		}
	}
	
	# Calculate degrees for each node
	degrees <- numeric(n)
	for (i in 1:n) {
		degrees[i] <- sum(m[i,])
	}

	if (graph == T) {
		truehist(degrees)
		lines(density(degrees))
	
		legend('topright', legend=c(paste('Mean degree:', round(mean(degrees), 
																3)),
									paste('n:', n),
									paste('p:', p)))
	}
	
	list(mtrx=m, degrees=degrees)
}

# Test
rGraph(200, .01, F)

# For a range of n and p run and store in a datafram (n, p, mean(degrees)), then 
# plot as heat map
iters <- 100
ns <- ps <- meanDegrees <- numeric(iters)
for (i in 1:iters) {
	ns[i] <- as.integer(runif(1, 1, 100))
	ps[i] <- runif(1, 0, 0.08)
	md <- rGraph(ns[i], ps[i], F)
	meanDegrees[i] <- mean(md$degrees)
}

data <- data.frame(ns, ps, meanDegrees)

# Graph