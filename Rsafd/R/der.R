`der` <-
function(X,k=1)
# X has to be a timeSeries and k an integer
# if Y <- der(X,k) then Y[t]=X[t]-X[t-k]
{
    L <- length(seriesPositions(X))
    if (abs(k) > L) stop("Series cannot be differentiated")
    if (k>=0) 
    {
        Y <- timeSeries(charvec=seriesPositions(X)[(k+1):L],data=diff(seriesData(X),k))
    }
    else
    {
       k <- as.integer(abs(k))  
       Y <- timeSeries(charvec=seriesPositions(X)[1:(L-k)],data=diff(seriesData(X),k))
    }
    return(Y)
}

