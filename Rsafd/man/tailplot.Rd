\name{tailplot}
\alias{tailplot}

\title{GENERALIZED EXTREME VALUE DISTRIBUTION}

\description{
        Cumulative distribution function and quantiles for the distribution
        of a GPD object (as produced for example by the semi-parametric estimation 
        procedure \code{gpd.tail}).
}
      
\usage{
        tailplot(gpd.obj, tail="upper", optlog = NA, extend = 1.5, labels = T, ...)
}

\arguments{ 
       \item{gpd.obj}{An object of class \code{gpd} as created by \code{gpd.tail}.}
 \item{\strong{Optional Arguments}}{}
       \item{tail}{Character string which can be either "upper" or "lower" depending upon
            the choice of the upper or lower part of the data}
       \item{optlog}{Character string which can be "x", "y", "xy", "yx", "" or "NA" indicating 
            which axis if any, should be labelled an a logarithmic scale.}
       \item{extend}{Factor by which the x-axis should extend beyond the data} 
}


\examples{

# One tail
X <- rgpd(1024)
x.est <- gpd.tail(X)
tailplot(x.est)

}

\author{Rene Carmona, \email{rcarmona@princeton.edu}}
\keyword{hplot}
