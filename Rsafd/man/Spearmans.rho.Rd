\name{Spearmans.rho}
\alias{Spearmans.rho}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Spearman's Rho}
\description{
  Computes Spearman's Rho for an object of the class copula
}
\usage{
Spearmans.rho(copula, tol = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{copula}{\code{copula}}
  \item{tol}{ \code{tol}}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  a numerical value
}

\author{ Rene Carmona }

\seealso{\code{\link{Kendalls.tau}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(copula, tol = 1e-5) standardGeneric("Spearmans.rho")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{robust}