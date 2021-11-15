#THERE IS A BUILT IN R FUNCTION: power....(), e.g.:
#power.t.test(), power.anova.test(), power.prop.test()


#Determines sample size needed for a given confidence and acceptable error; for estimates of means or proportion (p) of indicator variables.  Assumes no knowledge of the variance.

sampleNeeded <- function(percent, error) {
	#percent is percent confidence represented as a decimal, error is acceptable error
	z <- qnorm((1+percent)/2);
	sampleSize <- (z/(2*error))^2
	sampleSize
	}	