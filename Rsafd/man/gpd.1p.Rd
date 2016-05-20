\name{gpd.cdf}
\alias{gpd.cdf}
\alias{gpd.1p}
\alias{gpd.1q}
\alias{gpd.2p}
\alias{gpd.2q}

\title{CUMULATIVE DISTRIBUTION AND QNTILE FUNCTIONS OF A GPD OBJECT}

\description{
        Cumulative distribution function and quantiles for the distribution
        of a GPD object (as produced for example by the semi-parametric estimation 
        procedure \code{gpd.tail}).
}

\usage{
       
         gpd.1p(x, est.object, linear = TRUE) 
         gpd.1q(p, est.object, linear = TRUE) 
         gpd.2p(x, est.object, linear = TRUE) 
         gpd.2q(p, est.object, linear = TRUE) 
}

\arguments{
       \item{x}{A numeric vector of values at which the cdf is computed}
       \item{p}{A numeric vector of probabilities at which the quantiles are computed}
       \item{est.object}{An object of class \code{gpd} as the output of \code{gpd.tail}.}
 \item{\strong{OPTIONAL ARGUMENTS}}{} 
          \item{linear}{A boolean. If \code{TRUE} (default), the empirical
          cdf and quantile function
          are linearly interpolated for \code{lower < x < upper}. If \code{FALSE},
          the empirical  cdf and quantile function
          are returned for \code{lower < x < upper}.}
}

\value{
       Functions \code{gpd.1p} and \code{gpd.2p} return a vector of the same 
       length as \code{x} comprising the values of the cumulative distribution 
       function of the distribution determined by \code{est.obj} computed at the
        points \code{x}.

       Functions \code{gpd.1q} and \code{gpd.2q} return a vector of the same 
       length as \code{q} comprising the values of the quantiles of the distribution 
       determined by \code{est.obj} computed at the points \code{q}.
}


\seealso{
 \code{\link{pgpd}}, \code{\link{qgpd}} 
}

\examples{
# One tail
data("BCofLRet")
NZ <- (BCofLRet !=0)
BLRet <- BCofLRet[NZ]
X <- BLRet[BLRet > 0]
x.est <- gpd.tail(X)

y <- c(10:500)/1000
plot(y, gpd.1p(y,x.est), log = "x", type = "l")
big.X <- sort(X) > 0.01
points((sort(X))[big.X], (ppoints(sort(X)))[big.X])

# random generation from this distribution:
data("BCofLRet")
NZ <- (BCofLRet !=0)
BLRet <- BCofLRet[NZ]
X <- BLRet[BLRet != 0]
x.est <- gpd.tail(X, upper = 0.015, lower = -0.015, method = "lmom")
n <- length(X)
Y <- gpd.2q(runif(n), x.est)
plot(X, ylim = c(-0.3,0.3))
plot(Y, col = 4, , ylim = c(-0.3,0.3))
}

\author{Rene Carmona, \email{rcarmona@princeton.edu}}
\keyword{distribution}
