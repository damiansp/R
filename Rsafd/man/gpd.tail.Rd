\name{gpd.tail}
\alias{gpd.tail}
\title{POT ESTIMATION OF A GENERALIZED PARETO DISTRIBUTION}

\description{
      Semi-parametric estimation based on the Peaks over Threshold (POT)
      method.of a distribution with upper and
      possibly lower tail have the same size as the tail 
      of a generalized Pareto distribution.
}
      
\usage{
       
         gpd.tail(data, one.tail=F, upper = NA, lower = NA, upper.method ="ml", lower.method="ml", plot = T, ...)

}

\arguments{ 
	\item{data}{A numerical vector of sample data values}
	\item{\strong{Optional Arguments}}{}
        	\item{one.tail}{Logical which should be set to \code{TRUE} when the 
            sample is limited to the left} 
       	\item{upper}{The value of the upper threshold. A Generalized Pareto 
           distribution is fitted to the data exceeding this threshold.
           If its value is missing, \code{upper} is estimated so that either
           15\% of the data samples, or exactly 150 values, whichever number is greater,
           are greater than the upper threshold.}
       \item{lower}{The value of the lower threshold. A Generalized Pareto 
           distribution is fitted to the data below this threshold.
          If its value is missing, it is estimated so that either
          15\% of the data samples, or exactly 150 values, whichever number is smaller,
           is smaller than the lower threshold.}
       \item{upper.method}{Character string which can be either "ml" or "lmom" depending upon
            the choice of the method used to estimate the parameters of the generalized
            Pareto distribution fitted to the part of the data exceeding \code{upper}}
       \item{lower.method}{Character string which can be either "ml" or "lmom" depending upon
            the choice of the method used to estimate the parameters of the generalized
            Pareto distribution fitted to the part of the data below \code{lower}}
       \item{plot}{If \code{TRUE} (default), for each tail, a plot of the excesses over threshold
           against the quantiles of the GPD with the same shape parameter
           is produced. The fact that the points line up supports the assumption about the
           (generalized) Pareto nature of the tail in question.}
}

\value{    
       The value of \code{gpd.tail} is an object of class \code{gpd}. It is a list
       containing fields: length of the data,
       sorted vector of data points, the value of upper threshold, and
       estimates of GPD shape and scale parameter for the tail.
       and  representing
       threshold and parameter estimates for the lower tail.
}


\note{ If \code{plot = TRUE} a plot of the exceedences over the threshold against the
     quantiles of the estimated GPD is produced for each of the tails studied.

The semiparametric estimation performed by this function was done by the functions
\code{pot.1tail.est} and \code{pot.2tails.est} in the original \code{EVANESCE} library.
These functions were included in the \code{FinMetrics} library of \code{S-Plus} under the
name of \code{gpd.tail} to avoid confusion with the similar function \code{gpd} contributed
by Mc Neil. We named it \code{gpd.tail} for backward compatibility
with \code{FinMetrics}.
}

\seealso{
\code{\link{gpd.lm}}, \code{\link{gpd.lmom}}
}

\examples{
# One tail
data("BCofLRet")
NZ <- (BCofLRet !=0)
BLRet <- BCofLRet[NZ]
X <- BLRet[BLRet > 0]
x.est <- gpd.tail(X,one.tail=TRUE,upper=0.04)

y <- c(10:500)/1000
plot(y, gpd.1p(y,x.est), log = "x", type = "l")
big.X <- sort(X) > 0.01
points((sort(X))[big.X], (ppoints(sort(X)))[big.X])

# random generation from this distribution:
X <- BLRet[BLRet != 0]
x.est <- gpd.tail(X, upper = 0.015, lower = -0.015, method = "lmom")
n <- length(X)
Y <- gpd.2q(runif(n), x.est)
plot(X, ylim = c(-0.3,0.3))
plot(Y, col = 4, ylim = c(-0.3,0.3))
}

\author{Rene Carmona, \email{rcarmona@princeton.edu}}
\keyword{estimation}
