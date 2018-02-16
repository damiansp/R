err.bars <- function(barV, errorV, barNames, ylab=deparse(substitute(barV))) {
	xv <- barplot(barV, ylim=c(0, (max(barV) + max(errorV))), names=barNames, ylab=ylab)
	g <- (max(xv) - min(xv)) / 50
	for(i in 1:length(xv)) {
		lines(c(xv[i], xv[i]), c(barV[i] + errorV[i], barV[i] - errorV[i]))
		lines(c(xv[i] - g, xv[i] + g), c(barV[i] + errorV[i], barV[i] + errorV[i]))
		lines(c(xv[i] - g, xv[i] + g), c(barV[i] - errorV[i], barV[i] - errorV[i]))
		}
	}