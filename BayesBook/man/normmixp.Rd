\name{normmixp}
\alias{normmixp}
\title{Bayesian inference on a normal mean with a mixture of normal priors}
\description{
  Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean of a normal
  distribution, with a mixture of normal priors on \eqn{\mu}{mu}
}
\usage{
normmixp(x, sigma.x, prior0, prior1, p = 0.5, n.mu = 100, ret = FALSE)
}
\arguments{
	\item{x}{a vector of observations from a normal distribution with unknown mean and known std. deviation.}
	\item{sigma.x}{the population std. deviation of the observations}	
	\item{prior0}{the vector of length 2 which contains the means
	  and standard deviation of your precise prior}
	\item{prior1}{the vector of length 2 which contains the means
	  and standard deviation of your vague prior}
	\item{n.mu}{the number of possible \eqn{\mu}{mu} values in the prior}
	\item{p}{the mixing proportion for the two component normal priors}
	\item{ret}{this argument is deprecated.}
}
\seealso{
  \code{\link{binomixp}}
  \code{\link{normdp}}
  \code{\link{normgcp}}
}
\value{
  A list will be returned with the following components:
  \item{mu}{the vector of possible \eqn{\mu}{mu} values used in the prior}
  \item{prior}{the associated probability mass for the values in \eqn{\mu}{mu}}
  \item{likelihood}{the scaled likelihood function for \eqn{\mu}{mu}
    given \eqn{x} and \eqn{\sigma_x}{sigma.x}}
  \item{posterior}{the posterior probability of \eqn{\mu}{mu} given
    \eqn{x} and \eqn{\sigma_x}{sigma.x}} 
}
\examples{
## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a N(0,1) prior on mu - a 50:50 mix of
## two N(0,1) densities
normmixp(x,1,c(0,1),c(0,1))

## find the posterior density with 50:50 mix of a N(0.5,3) prior and a
## N(0,1) prior on mu
normmixp(x,1,c(0.5,3),c(0,1))

## Find the posterior density for mu, given a random sample of 4 
## observations from N(mu,1), y = [2.99, 5.56, 2.83, 3.47], 
## and a 80:20 mix of a N(3,2) prior and a N(0,100) prior for mu
x = c(2.99,5.56,2.83,3.47)
normmixp(x,1,c(3,2),c(0,100),0.8)
}
\keyword{misc}

