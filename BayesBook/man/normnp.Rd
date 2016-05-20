\name{normnp}
\alias{normnp}
\title{Bayesian inference on a normal mean with a normal prior}
\description{
	Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean of a normal distribution, with a normal prior on \eqn{\mu}{mu}
}
\usage{
normnp(x, m.x = 0, s.x = 1, sigma.x = NULL, n.mu = 100, ret = FALSE)
}
\arguments{
	\item{x}{a vector of observations from a normal distribution with unknown mean and known std. deviation.}
	\item{m.x}{the mean of the normal prior}
	\item{s.x}{the standard deviation of the normal prior}
	\item{sigma.x}{the population std. deviation of the normal
	  distribution. If this value is NULL, which is is by default,
	  then a flat prior is used and m.x and s.x are ignored}	
	\item{n.mu}{the number of possible \eqn{\mu}{mu} values in the prior}
	\item{ret}{this argument is deprecated.}
}
\seealso{
	\code{\link{normdp}}
	\code{\link{normgcp}}
}
\value{
	A list will be returned with the following components:
	\item{mu}{the vector of possible \eqn{\mu}{mu} values used in the prior}
	\item{mu.prior}{the associated probability mass for the values in \eqn{\mu}{mu}}
	\item{likelihood}{the scaled likelihood function for
	  \eqn{\mu}{mu} given \eqn{x} and \eqn{\sigma_x}{sigma.x}}
	\item{posterior}{the posterior probability of \eqn{\mu}{mu}
	  given \eqn{x} and \eqn{\sigma_x}{sigma.x}}
	  \item{mean}{the posterior mean}
	  \item{sd}{the posterior standard deviation}
	  \item{qtls}{a selection of quantiles from the posterior density}
}
\examples{
## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a N(0,1) prior on mu
normnp(x,sigma=1)

## find the posterior density with N(0.5,3) prior on mu
normnp(x,0.5,3,1)

## Find the posterior density for mu, given a random sample of 4 
## observations from N(mu,sigma^2=1), y = [2.99, 5.56, 2.83, 3.47], 
## and a N(3,sd=2)$ prior for mu
y = c(2.99,5.56,2.83,3.47)
normnp(y,3,2,1)
}
\keyword{misc}

