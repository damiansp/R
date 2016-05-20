\name{normdp}
\alias{normdp}
\title{Bayesian inference on a normal mean with a discrete prior}
\description{
	Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean of a normal distribution, with a discrete prior on \eqn{\mu}{mu}
}
\usage{
normdp(x, sigma.x = NULL, mu = NULL, mu.prior = NULL, n.mu = 50,
	ret = FALSE)
}
\arguments{
  \item{x}{a vector of observations from a normal distribution with unknown mean and known std. deviation.}
  \item{sigma.x}{the population std. deviation of the normal distribution}
  \item{mu}{a vector of possibilities for the probability of success in
    a single trial. If mu is NULL then a uniform prior is used.}
  \item{mu.prior}{the associated prior probability mass.}
  \item{n.mu}{the number of possible \eqn{\mu}{mu} values in the prior}
  \item{ret}{this argument is deprecated.}
}
\seealso{
  \code{\link{normnp}}
  \code{\link{normgcp}}
}
\value{
  A list will be returned with the following components:
  \item{mu}{the vector of possible \eqn{\mu}{mu} values used in the prior}
  \item{mu.prior}{the associated probability mass for the values in \eqn{\mu}{mu}}
  \item{likelihood}{the scaled likelihood function for
    \eqn{\mu}{mu} given \eqn{x} and \eqn{\sigma_x}{sigma.x}}
  \item{posterior}{the posterior probability of \eqn{\mu}{mu} given \eqn{x} and \eqn{\sigma_x}{sigma.x}}
}
\examples{
## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a uniform prior on mu
normdp(x,1)

## find the posterior density with a non-uniform prior on mu
mu = seq(-3,3,by=0.1)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
normdp(x,1,mu,mu.prior)

## Let mu have the discrete distribution with 5 possible
## values, 2, 2.5, 3, 3.5 and 4, and associated prior probability of
## 0.1, 0.2, 0.4, 0.2, 0.1 respectively. Find the posterior 
## distribution after a drawing random sample of n = 5 observations 
## from a N(mu,1) distribution y = [1.52, 0.02, 3.35, 3.49, 1.82]
mu = seq(2,4,by=0.5)
mu.prior = c(0.1,0.2,0.4,0.2,0.1)
y = c(1.52,0.02,3.35,3.49,1.82)
normdp(y,1,mu,mu.prior)
}
\keyword{misc}

