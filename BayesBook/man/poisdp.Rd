\name{poisdp}
\alias{poisdp}
\title{Poisson sampling with a discrete prior}
\description{
  Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean rate
  of occurance in a Poisson process and a discrete prior on \eqn{\mu}{mu}
}
\usage{
poisdp(y.obs, mu, mu.prior, ret = FALSE)
}
\arguments{
	\item{y.obs}{a random sample from a Poisson distribution.}
	\item{mu}{a vector of possibilities for the mean rate of
	  occurance of an event over a finite period of space or time.}
	\item{mu.prior}{the associated prior probability mass.}
	\item{ret}{this argument is deprecated.}
}
\seealso{
	\code{\link{poisgamp}}
	\code{\link{poisgcp}}
}
\value{
  A list will be returned with the following components:
  
  \item{likelihood}{the scaled likelihood function for \eqn{\mu}{mu}
    given \eqn{y_{obs}}{y.obs}}
  \item{posterior}{the posterior probability of \eqn{\mu}{mu} given
    \eqn{y_{obs}}{y.obs}}
  \item{mu}{the vector of possible \eqn{\mu}{mu} values used in the prior}
  \item{mu.prior}{the associated probability mass for the values
    in \eqn{\mu}{mu}}
}
\examples{
## simplest call with an observation of 4 and a uniform prior on the
## values mu = 1,2,3
poisdp(4,1:3,c(1,1,1)/3)

##  Same as the previous example but a non-uniform discrete prior
mu = 1:3
mu.prior = c(0.3,0.4,0.3)
poisdp(4,mu=mu,mu.prior=mu.prior)

##  Same as the previous example but a non-uniform discrete prior
mu = seq(0.5,9.5,by=0.05)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
poisdp(4,mu=mu,mu.prior=mu.prior)

## A random sample of 50 observations from a Poisson distribution with
## parameter mu = 3 and  non-uniform prior
y.obs = rpois(50,3)
mu = c(1:5)
mu.prior = c(0.1,0.1,0.05,0.25,0.5)
results = poisdp(y.obs, mu, mu.prior)

##  Same as the previous example but a non-uniform discrete prior
mu = seq(0.5,5.5,by=0.05)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
y.obs = rpois(50,3)
poisdp(y.obs,mu=mu,mu.prior=mu.prior)

}
\keyword{misc}

