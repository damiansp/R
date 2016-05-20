\name{normgcp}
\alias{normgcp}
\title{Bayesian inference on a normal mean with a general continuous prior}
\description{
	Evaluates and plots the posterior density for \eqn{\mu}{mu}, the mean of a normal distribution, with a general continuous prior on \eqn{\mu}{mu}
}
\usage{
normgcp(x, sigma.x, density = "uniform" , params = NULL, n.mu = 50, mu =
NULL,mu.prior = NULL, ret = FALSE)
}
\arguments{
  \item{x}{a vector of observations from a normal distribution with unknown mean and known std. deviation.}
  \item{sigma.x}{the population std. deviation of the normal distribution}
  \item{density}{distributional form of the prior density can be one of:
    "normal", "unform", or "user".}
  \item{params}{if density = "normal" then params must contain at least a mean and possible a std. deviation. If a std. deviation is not specified then sigma.x will be used as the std. deviation of the prior. If density = "uniform" then params must contain a minimum and a maximum value for the uniform prior. If a maximum and minimum are not specified then a \eqn{U[0,1]} prior is used}
  \item{n.mu}{the number of possible \eqn{\mu}{mu} values in the prior}
  \item{mu}{a vector of possibilities for the probability of success in a single trial. Must be set if density="user"}
  \item{mu.prior}{the associated prior probability mass. Must be set if density="user"}
  \item{ret}{this argument is deprecated.}
}
\seealso{
  \code{\link{normdp}}
  \code{\link{normnp}}
}
\value{
  A list will be returned with the following components:
  \item{likelihood}{the scaled likelihood function for \eqn{\mu}{mu}
    given \eqn{x} and \eqn{\sigma_x}{sigma.x}}
  \item{posterior}{the posterior probability of \eqn{\mu}{mu} given
    \eqn{x} and \eqn{\sigma}{sigma.x}}
  \item{mu}{the vector of possible \eqn{\mu}{mu} values used in the prior}
  \item{mu.prior}{the associated probability mass for the values in \eqn{\mu}{mu}}
}
\examples{
## generate a sample of 20 observations from a N(-0.5,1) population
x = rnorm(20,-0.5,1)

## find the posterior density with a uniform U[-3,3] prior on mu
normgcp(x,1,params=c(-3,3))

## find the posterior density with a non-uniform prior on mu
mu = seq(-3,3,by=0.1)
mu.prior = rep(0,length(mu))
mu.prior[mu<=0] = 1/3+mu[mu<=0]/9
mu.prior[mu>0] = 1/3-mu[mu>0]/9
normgcp(x,1,density="user",mu=mu,mu.prior=mu.prior)

## find the CDF for the previous example and plot it
## Note the syntax for sintegral has changed
results = normgcp(x,1,density="user",mu=mu,mu.prior=mu.prior)
cdf = sintegral(mu,results$posterior,n.pts=length(mu))$cdf
plot(cdf,type="l",xlab=expression(mu[0])
             ,ylab=expression(Pr(mu<=mu[0])))

## use the CDF for the previous example to find a 95\%
## credible interval for mu. Thanks to John Wilkinson for this simplified code

lcb = cdf$x[with(cdf,which.max(x[y<=0.025]))]
ucb = cdf$x[with(cdf,which.max(x[y<=0.975]))]
cat(paste("Approximate 95\% credible interval : ["
           ,round(lcb,4)," ",round(ucb,4),"]\n",sep=""))

## use the CDF from the previous example to find the posterior mean
## and std. deviation
dens = mu*results$posterior
post.mean = sintegral(mu,dens)$value

dens = (mu-post.mean)^2*results$posterior
post.var = sintegral(mu,dens)$value
post.sd = sqrt(post.var)

## use the mean and std. deviation from the previous example to find
## an approximate 95\% credible interval
lb = post.mean-qnorm(0.975)*post.sd
ub = post.mean+qnorm(0.975)*post.sd


cat(paste("Approximate 95\% credible interval : ["
   ,round(lb,4)," ",round(ub,4),"]\n",sep=""))

}
\keyword{misc}

