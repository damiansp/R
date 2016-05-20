\name{eda.shape}
\alias{eda.shape}

\title{Empirical Data Analysis }
\description{
Four graphical representations of the empirical features of a numeric vector interpreted as a sample from a given statistical distribution}
\usage{
eda.shape(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric vector whose entries are interpreted as observations from a given distribution}
}

\value{
  Produce histogram, normal Q-Q plot, box-plot and kernel density estimator of the numerical vector given as parameter.
}
\references{ \code{S-Plus} help files }
\seealso{\code{\link{hist}}, \code{\link{boxplot}},\code{\link{qqplot}},\code{\link{density}},}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x)
{
    par(mfrow = c(2, 2))
    hist(x)
    boxplot(x)
    iqd <- summary(x)[5] - summary(x)[2]
    plot(density(x, width = 2 * iqd), xlab = "x", ylab = "", type = "l")
    qqnorm(x)
    qqline(x)
    par(mfrow = c(1, 1))
  }
}

\keyword{hplot}

