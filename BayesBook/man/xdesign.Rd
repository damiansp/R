\name{xdesign}
\alias{xdesign}
\title{Monte Carlo study of randomized and blocked designs}
\description{
	 Simulates completely randomized design and randomized block designs from a population of experimental units with underlying response values \eqn{y} and underlying other variable values \eqn{x} (possibly lurking)
}
\usage{
	xdesign(x = NULL, y = NULL, corr = 0.8, size = 20, n.treatments = 4
					, n.rep = 500)
}
\arguments{
	\item{x}{a set of lurking values which are correlated with the response}
	\item{y}{a set of response values}
	\item{corr}{the correlation between the response and lurking variable}
	\item{size}{the size of the treatment groups}
	\item{n.treatments}{the number of treatments}
	\item{n.rep}{the number of Monte Carlo replicates}
}
\value{
	If the ouput of xdesign is assigned to a variable, then a list is returned with the following components:
	\item{block.means}{a vector of the means of the lurking variable from each replicate of the simulation stored by treatment number within replicate number}
	\item{treat.means}{a vector of the means of the response variable from each replicate of the simulation stored by treatment number within replicate number}
	\item{ind}{a vector containing the treatment group numbers. Note that there will be twice as many group numbers as there are treatments corresponding to the simulations done using a completely randomized design and the simulations done using a randomized block design}
}
\examples{
# Carry out simulations using the default parameters 
xdesign()
# Carry out simulations using a simulated response with 5 treaments, groups of size 25, and a correlation of -0.6 between the response and lurking variable
xdesign(corr=-0.6,size=25,n.treatments=5)
}
\keyword{misc}


