\name{bayes.lin.reg}
\alias{bayes.lin.reg}
\title{Bayesian inference for simple linear regression}
\description{
  This function is used to find the posterior distribution of the
  simple linear regression slope variable \eqn{\beta}{beta} when we have
  a random sample of ordered pairs \eqn{(x_{i}, y_{i})} from the simple
  linear regression  model:
  \deqn{
    y_{i} = \alpha_{\bar{x}} + \beta x_{i}+\epsilon_{i}
    }{y_i = alpha_xbar + beta*x_i+epsilon_i}
  where the observation errors are, \eqn{\epsilon_i}{epsilon_i}, independent
  \eqn{normal(0,\sigma^{2})}{normal(0,sigma^2)} with known variance.
}
\usage{
bayes.lin.reg(y, x, slope.prior = "flat",intcpt.prior = "flat",
              mb0 = 0, sb0 = 0, ma0 = 0, sa0 = 0, sigma = NULL,
              alpha = 0.05, plot.data = FALSE, pred.x = NULL,
              ret = FALSE)
}
\arguments{
  \item{y}{the vector of responses.}
  \item{x}{the value of the explantory variable associated with each
    response.}
   \item{slope.prior}{use a ``flat'' prior or a ``normal'' prior. for
    \eqn{\beta}{beta}}
  \item{intcpt.prior}{use a ``flat'' prior or a ``normal'' prior. for
    \eqn{\alpha_[\bar{x}]}{alpha_xbar}}
  \item{mb0}{the prior mean of the simple linear regression slope
    variable \eqn{\beta}{beta}. This argument is ignored for a flat prior.}
  \item{sb0}{the prior std. deviation of the simple linear regression slope
    variable \eqn{\beta}{beta} - must be greater than zero. This
    argument is ignored for a flat prior.}
  \item{ma0}{the prior mean of the simple linear regression intercept
    variable \eqn{\alpha_{\bar{x}}}{alpha_xbar}. This argument is
    ignored for a flat prior.}
  \item{sa0}{the prior std. deviation of the simple linear regression
    variable \eqn{\alpha_{\bar{x}}}{alpha_xbar} - must be greater than
    zero. This argument is ignored for a flat prior.}
  \item{sigma}{the value of the std. deviation of the residuals. By
    default, this is assumed to be unknown and the sample value is used
    instead. This affects the prediction intervals.}
  \item{alpha}{controls the width of the credible interval.}
  \item{plot.data}{if true the data are plotted, and the posterior
    regression line superimposed on the data.}
  \item{pred.x}{a vector of x values for which the predicted y values
    are obtained and the std. errors of prediction}
  \item{ret}{this argument is deprecated.}
}
\value{
	A list will be returned with the following components:
	\item{post.coef}{the posterior mean of the intecept and the slope}
	\item{post.coef}{the posterior standard deviation of the intercept the slope}
	\item{pred.x}{the vector of values for which predictions have
	  been requested. If pred.x is NULL then this is not returned}
	\item{pred.y}{the vector predicted values corresponding to
	  pred.x. If pred.x is NULL then this is not returned}
	\item{pred.se}{The standard errors of the predicted values in
	  pred.y. If pred.x is NULL then this is not returned}
}
\examples{
## generate some data from a known model, where the true value of the
## intercept alpha is 2, the true value of the slope beta is 3, and the
## errors come from a normal(0,1) distribution
x = rnorm(50)
y = 22+3*x+rnorm(50)

## use the function with a flat prior for the slope beta and a
## flat prior for the intercept, alpha_xbar.

bayes.lin.reg(y,x)

## use the function with a normal(0,3) prior for the slope beta and a
## normal(30,10) prior for the intercept, alpha_xbar.

bayes.lin.reg(y,x,"n","n",0,3,30,10)

## use the same data but plot it and the credible interval

bayes.lin.reg(y,x,"n","n",0,3,30,10,plot.data = TRUE)

## The heart rate vs. O2 uptake example 14.1
O2 = c(0.47,0.75,0.83,0.98,1.18,1.29,1.40,1.60,1.75,1.90,2.23)
HR = c(94,96,94,95,104,106,108,113,115,121,131)
plot(HR,O2,xlab="Heart Rate",ylab="Oxygen uptake (Percent)")

bayes.lin.reg(y,x,"n","f",0,1,sigma=0.13)

## Repeat the example but obtain predictions for HR = 100 and 110

bayes.lin.reg(y,x,"n","f",0,1,sigma=0.13,pred.x=c(100,110))
}
\keyword{misc}

