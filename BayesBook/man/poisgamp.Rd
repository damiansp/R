\name{poisgamp}
\alias{poisgamp}
\title{Poisson sampling with a gamma prior}
\description{
	Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean rate
	of occurance in a Poisson process and a \eqn{gamma} prior on \eqn{\mu}{mu}
}
\usage{
poisgamp(y, r, v, ret = FALSE)
}
\arguments{
	\item{y}{a random sample from a Poisson distribution.}
	\item{r}{the shape parameter of the \eqn{gamma} prior.}
	\item{v}{the rate parameter of the \eqn{gamma} prior. Note that the
	  scale is \eqn{1/v}}
	\item{ret}{This argument is deprecated and has no effect}
      }
\seealso{
	\code{\link{poisdp}}
	\code{\link{poisgcp}}
}
\value{
  A list will be returned with the following components:

  \item{prior}{the prior density assigned to \eqn{\mu}{mu}}
  \item{likelihood}{the scaled likelihood function for \eqn{\mu}{mu}
    given \eqn{y}}
  \item{posterior}{the posterior probability of \eqn{\mu}{mu} given \eqn{y}}
  \item{r}{the shape parameter for the \eqn{gamma} posterior}
  \item{v}{the rate parameter for the \eqn{gamma} posterior}
}
\examples{
## simplest call with an observation of 4 and a gamma(1,1), i.e. an exponential prior on the
## mu
poisgamp(4,1,1)

##  Same as the previous example but a gamma(10,1) prior
poisgamp(4,10,1)

##  Same as the previous example but an improper gamma(1,0) prior
poisgamp(4,1,0)

## A random sample of 50 observations from a Poisson distribution with
## parameter mu = 3 and  gamma(6,3) prior
y = rpois(50,3)
poisgamp(y,6,3)

## In this example we have a random sample from a Poisson distribution
## with an unknown mean. We will use a gamma(6,3) prior to obtain the
## posterior gamma distribution, and use the R function qgamma to get a
## 95\% credible interval for mu
y = c(3,4,4,3,3,4,2,3,1,7)
results = poisgamp(y,6,3)
ci = qgamma(c(0.025,0.975),results$r, results$v)
cat(paste("95\% credible interval for mu: [",round(ci[1],3), ",", round(ci[2],3)),"]\n")

}
\keyword{misc}

