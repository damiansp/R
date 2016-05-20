empirical.copula <- function(x, y = NA)
{
  data <- NA
  if(inherits(x,"empirical.copula")) {data <- x}
	else if ( is.list(x))
	{
	   temp <- data.frame(x)
	   data <- new("empirical.copula",x = temp[,1], y = temp[,2])
	}
	else if (!is.na(y[1]) & length(x) == length(y)) 
	{
	   data <- new("empirical.copula",x = x, y = y)
	}
	else 
	{
	  stop("Can't create empirical copula. Unknown parameters supplied")
	}
	
	inputsOK <- T
	message <- ""
	if (sum(data@x < 0) > 0 | sum(data@y < 0) > 0) 
	{
		inputsOK <- F
		message <- 
		   paste(message," At least one of the observed data points has a negative coordinate", sep = " \n") 
	} 
	if (sum(data@x > 1) > 0 | sum(data@y > 1) > 0) 
	{
		inputsOK <- F
		message <- 
		   paste(message," At least one of the observed data points has a coordinate greater than 1", 
		          sep = " \n") 
	} 
	if (!inputsOK) {stop(message)}
    	 
	data  	
}
