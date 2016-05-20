\name{Kendalls.tau}
\alias{Kendalls.tau}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Kendall's Tau}
\description{
  Computes Kendalls Tau for an object of the class copula
}
\usage{
Kendalls.tau(copula, tol = 1e-05)
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

\seealso{\code{\link{Spearmans.rho}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(copula, tol = 1e-5) standardGeneric("Kendalls.tau")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{robust}

