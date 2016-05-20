\name{binodp}
\alias{binodp}
\title{Binomial sampling with a discrete prior}
\description{
  Evaluates and plots the posterior density for \eqn{\pi}{pi}, the probability of a success in a Bernoulli trial, with binomial sampling and a discrete prior on \eqn{\pi}{pi}
}
\usage{
binodp(x,n, pi = NULL, pi.prior = NULL, n.pi = 10, ret = FALSE)
}
\arguments{
  \item{x}{the number of observed successes in the binomial experiment.}
  \item{n}{the number of trials in the binomial experiment.}
  \item{pi}{a vector of possibilities for the probability of
    success in a single trial. if \code{pi} is \code{NULL} then a discrete
    uniform prior for \eqn{\pi}{pi} will be used.}
  \item{pi.prior}{the associated prior probability mass.}
  \item{n.pi}{the number of possible \eqn{\pi}{pi} values in the prior}
  \item{ret}{this argument is deprecated.}
}
\seealso{
	\code{\link{binobp}}
	\code{\link{binogcp}}
}
\value{
  A list will be returned with the following components:
  \item{pi}{the vector of possible \eqn{\pi}{pi} values used in the prior}
   \item{pi.prior}{the associated probability mass for the values in
     \eqn{\pi}{pi}}
   \item{likelihood}{the scaled likelihood function for \eqn{\pi}{pi} given
    \eqn{x} and \eqn{n}}
  \item{posterior}{the posterior probability of \eqn{\pi}{pi} given
    \eqn{x} and \eqn{n}}
  \item{f.cond}{the conditional distribution of \eqn{x} given
    \eqn{\pi}{pi} and \eqn{n}}
  \item{f.joint}{the joint distribution of \eqn{x} and \eqn{\pi}{pi}
    given \eqn{n}}
  \item{f.marg}{the marginal distribution of \eqn{x}}
}
\examples{
## simplest call with 6 successes observed in 8 trials and a uniform prior
binodp(6,8)

## same as previous example but with more possibilities for pi
binodp(6,8,n.pi=100)

## 6 successes, 8 trials and a non-uniform discrete prior
pi = seq(0,1,by=0.01)
pi.prior = runif(101)
pi.prior = sort(pi.prior/sum(pi.prior))
binodp(6,8,pi,pi.prior)

## 5 successes, 6 trials, non-uniform prior
pi = c(0.3,0.4,0.5)
pi.prior = c(0.2,0.3,0.5)
results = binodp(5,6,pi,pi.prior)

## plot the results from the previous example using a side-by-side barplot
results.matrix = rbind(results$pi.prior,results$posterior)
colnames(results.matrix) = pi
barplot(results.matrix,col=c("red","blue"),beside=TRUE
	,xlab=expression(pi),ylab=expression(Probability(pin)))
box()
legend(1,0.65,legend=c("Prior","Posterior"),fill=c("red","blue"))
}
\keyword{misc}

