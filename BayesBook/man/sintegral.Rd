\name{sintegral}
\alias{sintegral}
\title{Numerical integration using Simpson's Rule}
\description{
  Takes a vector of \eqn{x} values and a corresponding set of postive
  \eqn{f(x)=y} values, or a function, and evaluates the area under the curve:
  \deqn{
    \int{f(x)dx}
    }.
}
\usage{
	sintegral(x, fx, n.pts = 256, ret = FALSE)
}
\arguments{
	\item{x}{a sequence of \eqn{x} values.}
	\item{fx}{the value of the function to be integrated at \eqn{x}  or a function}
	\item{n.pts}{the number of points to be used in the integration.}
	\item{ret}{this argument is deprecated (and ignored)}
      }
      \value{
	A list containing two elements, \code{value} - the value of the
	intergral, and \code{cdf} - a list containing elements x and y
	which give a numeric specification of the cdf.
      }
\examples{
## integrate the normal density from -3 to 3
x = seq(-3, 3, length = 100)
fx = dnorm(x)
estimate = sintegral(x,fx)$value
true.val = diff(pnorm(c(-3,3)))
abs.error = abs(estimate-true.val)
rel.pct.error =  100*abs(estimate-true.val)/true.val
cat(paste("Absolute error :",round(abs.error,7),"\n"))
cat(paste("Relative percentage error :",round(rel.pct.error,6),"percent\n"))

## repeat the example above using dnorm as function
x = seq(-3, 3, length = 100)
estimate = sintegral(x,dnorm)$value
true.val = diff(pnorm(c(-3,3)))
abs.error = abs(estimate-true.val)
rel.pct.error =  100*abs(estimate-true.val)/true.val
cat(paste("Absolute error :",round(abs.error,7),"\n"))
cat(paste("Relative percentage error :",round(rel.pct.error,6)," percent\n"))

## use the cdf

cdf = sintegral(x,dnorm)$cdf
plot(cdf, type = 'l', col = "black")
lines(x, pnorm(x), col = "red", lty = 2)

## integrate the function x^2-1 over the range 1-2
x = seq(1,2,length = 100)
sintegral(x,function(x){x^2-1})$value

## compare to integrate
integrate(function(x){x^2-1},1,2)

}
\keyword{misc}


