# code for adding error bars
error.bar <- function(barp, meas, SEupper, lower=SEupper, length=0.1, ...) {
  if (length(barp) != length(meas) 
      | length(meas) != length(lower) 
      | length(lower) != length(SEupper)) {
	stop("vectors must be same length")    	
  }	
  arrows(
    barp, meas + SEupper, barp, meas - lower, angle=90, code=3, length=length, ...)
}