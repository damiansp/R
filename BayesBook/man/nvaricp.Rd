\name{nvaricp}
\alias{nvaricp}
\title{Bayesian inference for a normal standard deviation with a scaled
  inverse chi-squared distribution}
\description{
  Evaluates and plots the posterior density for \eqn{\sigma}{sigma}, the
  standard deviation of a Normal distribution where the mean
  \eqn{\mu}{mu} is known
}
\usage{
nvaricp(y, mu, S0, kappa, cred.int = FALSE, alpha = 0.05, ret = FALSE)
}
\arguments{
	\item{y}{a random sample from a \eqn{normal(\mu,\sigma^2)}{normal(mu,sigma^2)} distribution.}
	\item{mu}{the known population mean of the random sample.}
	\item{S0}{the prior scaling factor.}
	\item{kappa}{the degrees of freedom of the prior.}
	\item{cred.int}{if TRUE then a 100(1-alpha) percent credible
	  interval will be calculated for \eqn{\sigma}{sigma}}
	\item{alpha}{controls the width of the credible
	  interval. Ignored if cred.int is FALSE}
	\item{ret}{this argument is deprecated.}
}
\value{
  A list will be returned with the following components:

  \item{sigma}{the vaules of \eqn{\sigma}{sigma} for which the prior,
    likelihood and posterior have been calculated}
  \item{prior}{the prior density for \eqn{\sigma}{sigma}}
  \item{likelihood}{the likelihood function for \eqn{\sigma}{sigma}
    given \eqn{y}{y}}
  \item{posterior}{the posterior density of \eqn{\mu}{sigma} given
    \eqn{y}{y}}
  \item{S1}{the posterior scaling constant}
  \item{kappa1}{the posterior degrees of freedom}
}
\examples{
## Suppose we have five observations from a normal(mu, sigma^2)
## distribution mu = 200 which are 206.4, 197.4, 212.7, 208.5.
y = c(206.4, 197.4, 212.7, 208.5, 203.4)

## We wish to choose a prior that has a median of 8. This happens when
## S0 = 29.11 and kappa = 1
nvaricp(y,200,29.11,1)

##  Same as the previous example but a calculate a 95\% credible
## interval for sigma
nvaricp(y,200,29.11,1,cred.int=TRUE)

##  Same as the previous example but a calculate a 95\% credible
## interval for sigma by hand. Note that the syntax of sintegral has
## changed
results = nvaricp(y,200,29.11,1,cred.int=TRUE)
attach(results)
cdf = sintegral(sigma,posterior)$cdf
Finv = approxfun(cdf$y,cdf$x)
lb = Finv(0.025)
ub = Finv(0.975)
cat(paste("95\% credible interval for sigma: [",
              signif(lb,4),", ", signif(ub,4),"]\n",sep=""))

}
\keyword{misc}

