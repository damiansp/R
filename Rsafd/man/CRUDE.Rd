\name{CRUDE}
\alias{CRUDE}
\docType{data}
\title{Price of future contracts of crude oil}
\description{
\code{CRUDE} is a 335 x 12 numeric matrix, each row corresponds to a date, the first one being 4/18/1989, and the last one being 8/12/2002.  Each row of the matrix \code{CRUDE} gives the prices of the 12 futures contracts of crude oil as traded the day before. Two rows are used in the text as illustration of the nonlinear least squares function \code{nls} The whole matrix is used in a problem at the end of Chapter 4.
}
\usage{data(CRUDE)}
\format{
  The format is:
 num [1:3325, 1:12] 19.8 19.6 19.5 19.5 19.9 ...
}
\details{
         [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]
[1,] 19.82000 19.66880 19.54168 19.42812 19.28439 19.14347 19.03871 18.83252 18.88824
[2,] 19.57000 19.46624 19.35805 19.23260 19.11129 18.98027 18.80725 18.74761 18.67286
[3,] 19.50000 19.50520 19.29647 19.17074 19.04335 18.91672 18.79745 18.67402 18.58153
[4,] 19.52000 19.50300 19.32748 19.18239 19.04329 18.90528 18.76494 18.63013 18.52529
[5,] 19.86000 19.73784 19.55092 19.36827 19.20584 19.04497 18.85133 18.72691 18.62329
[6,] 19.90742 19.80268 19.60836 19.41376 19.25118 19.09046 18.89778 18.79368 18.69129
        [,10]    [,11]    [,12]
[1,] 18.77467 18.83010 18.81668
[2,] 18.59192 18.52490 18.73277
[3,] 18.54249 18.52469 18.51315
[4,] 18.46776 18.44435 18.43283
[5,] 18.57120 18.55402 18.54250
[6,] 18.63583 18.61369 18.60218

}
\source{
R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag
}
\references{
R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag, Chapter 4.
}
\examples{
data(CRUDE)
}
\keyword{datasets}
