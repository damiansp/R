\name{binobp}
\alias{binobp}
\title{Binomial sampling with a beta prior}
\description{
	Evaluates and plots the posterior density for \eqn{\pi}{pi}, the probability of a success in a Bernoulli trial, with binomial sampling and a continous \eqn{beta(a,b)} prior.
}
\usage{
binobp(x, n, a = 1, b = 1, ret = FALSE)
}
\arguments{
	\item{x}{the number of observed successes in the binomial experiment.}
	\item{n}{the number of trials in the binomial experiment.}
	\item{a}{parameter for the beta prior - must be greater than zero}
	\item{b}{parameter for the beta prior - must be greater than zero}
	\item{ret}{this argument is deprecated.}
}
\seealso{
	\code{\link{binodp}}
	\code{\link{binogcp}}
}
\value{
  A list will be returned with the following components:
  \item{posterior}{the posterior density of \eqn{\pi}{pi} given \eqn{x}
  and \eqn{n} - i.e. the \eqn{beta(a+x,b+n-x)} density}
\item{likelihood}{the likelihood of \eqn{x} given \eqn{\pi}{pi} and
  \eqn{n}, i.e. the \eqn{binomial(n,\pi)}{binomial(n,pi)} density}
\item{prior}{the prior density of \eqn{\pi}{pi}, i.e. the
  \eqn{beta(a,b)} density}
  \item{pi}{the values of \eqn{\pi}{pi} for which the posterior density was evaluated}
  \item{mean}{the posterior mean}
  \item{var}{the posterior variance}
  \item{sd}{the posterior std. deviation}
  \item{quantiles}{a set of quantiles from the posterior}
}
\examples{
## simplest call with 6 successes observed in 8 trials and a beta(1,1) uniform
## prior
binobp(6,8)

## 6 successes observed in 8 trials and a non-uniform beta(0.5,6) prior
binobp(6,8,0.5,6)

## 4 successes observed in 12 trials with a non uniform beta(3,3) prior
## plot the stored prior, likelihood and posterior
results = binobp(4,12,3,3)

par(mfrow=c(3,1))
y.lims = c(0,1.1*max(results$posterior,results$prior))

plot(results$pi,results$prior,ylim=y.lims,type="l"
	,xlab=expression(pi),ylab="Density",main="Prior")
polygon(results$pi,results$prior,col="red")

plot(results$pi,results$likelihood,ylim=c(0,0.25),type="l"
	,xlab=expression(pi),ylab="Density",main="Likelihood")
polygon(results$pi,results$likelihood,col="green")

plot(results$pi,results$posterior,ylim=y.lims,type="l"
	,xlab=expression(pi),ylab="Density",main="Posterior")
polygon(results$pi,results$posterior,col="blue")



}
\keyword{misc}

