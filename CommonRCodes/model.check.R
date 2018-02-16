model.check <- function(obj, ...) {
	#This function should be used in conjunction with plot(model)
	rs <- obj$resid #residuals
	fv <- obj$fitted #fitted values
	par(mfrow=c(1,2))
	plot(fv, rs, xlab="Fitted Values", ylab="Residuals")
	abline(h=0, lty=2)
	qqnorm(rs, xlab="Normal Scores", ylab="Ordered Residuals")
	qqline(rs, lty=3)
	par(mfrow=c(1,1))
	invisible(NULL)
	}