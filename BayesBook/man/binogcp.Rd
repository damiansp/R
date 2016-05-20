\name{binogcp}
\alias{binogcp}
\title{Binomial sampling with a general continuous prior}
\description{
	Evaluates and plots the posterior density for \eqn{\pi}{pi}, the probability of a success in a Bernoulli trial, with binomial sampling and a general continuous prior on \eqn{\pi}{pi}
}
\usage{
binogcp(x, n, density = "uniform", params = c(0,1), n.pi = 1000,
	pi = NULL, pi.prior = NULL, ret = FALSE)
}
\arguments{
  \item{x}{the number of observed successes in the binomial experiment.}
  \item{n}{the number of trials in the binomial experiment.}
  \item{density}{may be one of "beta", "exp", "normal", "student", "uniform" or "user"}
  \item{params}{if density is one of the parameteric forms then
    then a vector of parameters  must be supplied.
    beta:		a,b
    exp:		rate
    normal: 	mean,sd
    uniform: 	min,max}
  \item{n.pi}{the number of possible \eqn{\pi}{pi} values in the prior}
  \item{pi}{a vector of possibilities for the probability of success in a single trial. This must be set if density="user".}
  \item{pi.prior}{the associated prior probability mass. This must be set if density="user".}
  \item{ret}{this argument is deprecated.}
}
\seealso{
	\code{\link{binobp}}
	\code{\link{binodp}}
}
\value{
  A list will be returned with the following components:
  \item{likelihood}{the scaled likelihood function for
    \eqn{\pi}{pi} given \eqn{x} and \eqn{n}}
  \item{posterior}{the posterior probability of \eqn{\pi}{pi} given \eqn{x} and \eqn{n}}
  \item{pi}{the vector of possible \eqn{\pi}{pi} values used in the prior}
  \item{pi.prior}{the associated probability mass for the values in \eqn{\pi}{pi}}
}
\examples{
## simplest call with 6 successes observed in 8 trials and a continuous
## uniform prior
binogcp(6,8)

## 6 successes, 8 trials and a Beta(2,2) prior
binogcp(6,8,density="beta",params=c(2,2))

## 5 successes, 10 trials and a N(0.5,0.25) prior
binogcp(5,10,density="normal",params=c(0.5,0.25))

## 4 successes, 12 trials with a user specified triangular continuous prior
pi = seq(0,1,by=0.001)
pi.prior = rep(0,length(pi))
pi.prior[pi<=0.5] = 4*pi[pi<=0.5]
pi.prior[pi>0.5] = 4-4*pi[pi>0.5]
results = binogcp(4,12,"user",pi=pi,pi.prior=pi.prior)

## find the posterior CDF using the previous example and Simpson's rule
## Please note that the syntax of sintegral has changed since the
## second edition of the book
cdf = sintegral(pi,results$posterior,n.pts=length(pi))$cdf
plot(cdf,type="l",xlab=expression(pi[0])
	,ylab=expression(Pr(pi<=pi[0])))

## use the cdf to find the 95\% credible region.
## Thanks to John Wilkinson for this simplified code.
lcb = cdf$x[with(cdf,which.max(x[y<=0.025]))]
ucb = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95\% credible interval : ["
	,round(lcb,4)," ",round(ucb,4),"]\n",sep=""))

## find the posterior mean, variance and std. deviation
## using Simpson's rule and the output from the previous example
dens = pi*results$posterior # calculate pi*f(pi | x, n)
post.mean = sintegral(pi,dens)$value

dens = (pi-post.mean)^2*results$posterior
post.var = sintegral(pi,dens)$value
post.sd = sqrt(post.var)

# calculate an approximate 95\% credible region using the posterior mean and
# std. deviation
lb = post.mean-qnorm(0.975)*post.sd
ub = post.mean+qnorm(0.975)*post.sd

cat(paste("Approximate 95\% credible interval : ["
	,round(lb,4)," ",round(ub,4),"]\n",sep=""))
}
\keyword{misc}


