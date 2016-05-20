\name{shape.plot}
\alias{shape.plot}

\title{ESTIMATE OF XI AS FUNCTION OF THE THRESHOLD}

\description{
        Plot of estimates of the shape parameter of one tail of the data.
}

\usage{
         shape.plot(data, tail="upper", method = "ml", from = 0.5, to = 0.98, nint = 30)}

\arguments{
       \item{data}{A numeric vector of values at which the cdf is computed}
       \item{tail}{Character string which can be either "upper" or "lower" depending upon
            the choice of the upper or lower part of the data}
       \item{method}{Character string which can be either "ml" or "lmom" depending upon
            the choice of the method used to estimate the shape parameter}
       \item{from}{Percentage of the data values left as not belonging to the tail}
       \item{to}{Maximum percentage of the data values excluded from the tail}
       \item{nint}{Integer giving the number of thresholds and hence the number of estimates
            of the shape parameter}
}

\value{
        Plot of the estimates of the shape parameter as function of the threshold
        (negative of the threshold when code{tail="lower"}). The percentages of points
        exceeding the threshold are given on the upper part of the plot.
}

\note{
        Based on Mc Neil's original \code{shape} function.
}

\seealso{
    \code{\link{gpd.tail}} 
}

\examples{
\dontrun{
data("BCofLRet")
NZ <- (BCofLRet !=0)
BLRet <- BCofLRet[NZ]

shape.plot(BLRet)
shape.plot(BLRet,tail="lower")
}
}
\author{Rene Carmona, \email{rcarmona@princeton.edu}}
\keyword{hplot}
