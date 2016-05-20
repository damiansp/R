# normalize.r
# Takes an arbitrary distribution and normalizes it on the basis of quantiles.
# E.g., given a beta distribution, its median is mapped to the median of a
# standard normal distribution; similarly with each quantile
# @author Damian Satterthwaite-Phillips
# @version 13 Jan 2013

# @param x A vector of data or distribution points
# @return A mapping of x to a standard normal distribution (ordering is 
#  presered)
# The max value of x is trimmed, as the 100% quantiles of 
#  the normal is infinity
normalizeDistribution <- function(x, graph=T) {
	xOrder <- order(x)
	restoreX <- order(xOrder)
	xOrdered <- x[xOrder]
	xQuants <- seq(0, 1, length.out = (length(x) + 1))
	xQuantsTrimmed <- c(xQuants[2:(length(xQuants) - 1)], NA)
	xNorm <- qnorm(xQuantsTrimmed)

	if (graph == T) {
		par(mfrow=c(2,2))
		hist(x, xlab='x (untrimmed)', main='')
		hist(xOrdered[2:(length(x) - 1)], xlab='x (trimmed)', main='')
		hist(xNorm, xlab='x Normalized', main='')
		qqnorm(xOrdered[2:(length(x) - 1)], type='l', lwd=2)
		qqline(x[2:(length(x) - 1)], col=2)
		par(mfrow=c(1,1))
	}
	xNorm[restoreX]
}

# Rescale a distribution to be on [0, 1]
normalize01 <- function(x, xmin = min(x), xmax = max(x)) {
	return ((x - xmin) / (xmax - xmin))
}

# Normalize such that all values sum to 1
normalize <- function(x) {
	return (x / sum(x))
}