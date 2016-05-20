\name{poisgcp}
\alias{poisgcp}
\title{Poisson sampling with a general continuous prior}
\description{
  Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean rate
  of occurance of an event or objects, with Poisson sampling and a
  general continuous prior on \eqn{\mu}{mu}
}
\usage{
poisgcp(y, density = "normal", params=c(0,1), n.mu = 100
       ,mu = NULL, mu.prior = NULL,
       print.sum.stat=FALSE, alpha=0.05, ret=FALSE)
}
\arguments{
	\item{y}{A random sample of one or more observations from a
	  Poisson distribution}
	\item{density}{may be one of "gamma", "normal", or "user"}
	\item{params}{if density is one of the parameteric forms then
		then a vector of parameters  must be supplied.
		gamma:		a0,b0
		normal: 	mean,sd}
	\item{n.mu}{the number of possible \eqn{\mu}{mu} values in the prior. This
	  number must be greater than or equal to 100. It is ignored
	  when density="user".}
	\item{mu}{a vector of possibilities for the mean of a Poisson distribution. This must be set if density="user".}
	\item{mu.prior}{the associated prior density. This must be set if density="user".}
	\item{print.sum.stat}{if set to TRUE then the posterior mean,
	  posterior variance, and a credible interval for the mean are
	  printed. The width of the credible interval is controlled by
	  the parameter alpha.}
	\item{alpha}{The width of the credible interval is controlled by
	  the parameter alpha.}
	\item{ret}{this arguement is deprecated.}
}
\seealso{
	\code{\link{poisdp}}
	\code{\link{poisgamp}}
}
\value{
	A list will be returned with the following components:
	\item{mu}{the vector of possible \eqn{\mu}{mu} values used in the prior}
	\item{mu.prior}{the associated probability mass for the values in \eqn{\mu}{mu}}
	\item{likelihood}{the scaled likelihood function for
	  \eqn{\mu}{mu} given \eqn{y}}
	\item{posterior}{the posterior probability of \eqn{\mu}{mu} given \eqn{y}}
}
\examples{
## Our data is random sample is 3, 4, 3, 0, 1. We will try a normal
## prior with a mean of 2 and a standard deviation of 0.5.
y = c(3,4,3,0,1)
poisgcp(y,density="normal",params=c(2,0.5))

## The same data as above, but with a gamma(6,8) prior
y = c(3,4,3,0,1)
poisgcp(y,density="gamma",params=c(6,8))

## The same data as above, but a user specified continuous prior.
## We will use print.sum.stat to get a 99\% credible interval for mu.
y = c(3,4,3,0,1)
mu = seq(0,8,by=0.001)
mu.prior = c(seq(0,2,by=0.001),rep(2,1999),seq(2,0,by=-0.0005))/10
poisgcp(y,"user",mu=mu,mu.prior=mu.prior,print.sum.stat=TRUE,alpha=0.01)

## find the posterior CDF using the results from the previous example
## and Simpson's rule. Note that the syntax of sintegral has changed.
results = poisgcp(y,"user",mu=mu,mu.prior=mu.prior)
cdf = sintegral(mu,results$posterior,n.pts=length(mu))$cdf
plot(cdf,type="l",xlab=expression(mu[0])
	,ylab=expression(Pr(mu<=mu[0])))

## use the cdf to find the 95\% credible region.
lcb = cdf$x[with(cdf,which.max(x[y<=0.025]))]
ucb = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95\% credible interval : ["
	,round(lcb,4)," ",round(ucb,4),"]\n",sep=""))

## find the posterior mean, variance and std. deviation
## using Simpson's rule and the output from the previous example
dens = mu*results$posterior # calculate mu*f(mu | x, n)
post.mean = sintegral(mu,dens)$value

dens = (mu-post.mean)^2*results$posterior
post.var = sintegral(mu,dens)$value
post.sd = sqrt(post.var)

# calculate an approximate 95\% credible region using the posterior mean and
# std. deviation
lb = post.mean-qnorm(0.975)*post.sd
ub = post.mean+qnorm(0.975)*post.sd

cat(paste("Approximate 95\% credible interval : ["
	,round(lb,4)," ",round(ub,4),"]\n",sep=""))
}
\keyword{misc}


