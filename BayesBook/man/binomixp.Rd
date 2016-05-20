\name{binomixp}
\alias{binomixp}
\title{Binomial sampling with a beta mixture prior}
\description{
	  Evaluates and plots the posterior density for \eqn{\pi}{pi}, the
	  probability of a success in a Bernoulli trial, with binomial
	  sampling when the prior density for \eqn{\pi}{pi} is a mixture of two
	  beta distributions, \eqn{beta(a_0,b_0)} and \eqn{beta(a_1,b_1)}.
}
\usage{
binomixp(x, n, alpha0=c(1,1), alpha1=c(1,1), p=0.5, ret = FALSE)
}
\arguments{
  \item{x}{the number of observed successes in the binomial experiment.}
  \item{n}{the number of trials in the binomial experiment.}
  \item{alpha0}{a vector of length two containing the parameters,
    \eqn{a_0}{a0} and \eqn{b_0}{b0}, for the first component beta prior - must be
    greater than zero. By default the elements of alpha0 are set
    to 1.}
  \item{alpha1}{a vector of length two containing the parameters,
    \eqn{a_1}{a1} and \eqn{b_1}{b1}, for the second component beta prior - must be
    greater than zero. By default the elements of alpha1 are set
    to 1.}
  \item{p}{The prior mixing proportion for the two component beta
    priors. That is the prior is
    \eqn{p\times beta(a_0,b_0)+(1-p)\times beta(a_1,b_1)}{p*beta(a0,b0)+(1-p)*beta(a1,b1)}. \eqn{p} is set to 0.5 by default}
  \item{ret}{this argument is deprecated.}
}
\seealso{
	\code{\link{binodp}}
	\code{\link{binogcp}}
	\code{\link{normmixp}}
}
\value{
	A list will be returned with the following components:
	\item{pi}{the values of \eqn{\pi}{pi} for which the posterior density was evaluated}	\item{posterior}{the posterior density of \eqn{\pi}{pi} given \eqn{n} and \eqn{x}}
	\item{likelihood}{the likelihood function for \eqn{\pi}{pi}
	  given \eqn{x} and \eqn{n}, i.e. the \eqn{binomial(n,\pi)}{binomial(n,pi)} density}
	\item{prior}{the prior density of \eqn{\pi}{pi} density}

}
\examples{
## simplest call with 6 successes observed in 8 trials and a 50:50 mix
## of two beta(1,1) uniform priors
binomixp(6,8)

## 6 successes observed in 8 trials and a 20:80 mix of a non-uniform
## beta(0.5,6) prior and a uniform beta(1,1) prior
binomixp(6,8,alpha0=c(0.5,6),alpha1=c(1,1),p=0.2)

## 4 successes observed in 12 trials with a 90:10 non uniform beta(3,3) prior
## and a non uniform beta(4,12).
## Plot the stored prior, likelihood and posterior
results = binomixp(4,12,c(3,3),c(4,12),0.9)

par(mfrow=c(3,1))
y.lims = c(0,1.1*max(results$posterior,results$prior))

plot(results$pi,results$prior,ylim=y.lims,type="l"
	,xlab=expression(pi),ylab="Density",main="Prior")
polygon(results$pi,results$prior,col="red")

plot(results$pi,results$likelihood,type="l"
	,xlab=expression(pi),ylab="Density",main="Likelihood")
polygon(results$pi,results$likelihood,col="green")

plot(results$pi,results$posterior,ylim=y.lims,type="l"
	,xlab=expression(pi),ylab="Density",main="Posterior")
polygon(results$pi,results$posterior,col="blue")



}
\keyword{misc}

