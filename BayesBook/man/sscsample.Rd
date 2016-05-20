\name{sscsample}
\alias{sscsample}
\title{Simple, Stratified and Cluster Sampling}
\description{
	Samples from a fixed population using either simple random sampling, stratitified sampling or cluster sampling.
}
\usage{
sscsample(size, n.samples, sample.type="simple", x = NULL,
          strata = NULL, cluster = NULL, ret=FALSE, print = TRUE)
}
\arguments{
	\item{size}{the desired size of the sample}
	\item{n.samples}{the number of repeat samples to take}
	\item{sample.type}{the sampling method. Can be one of "simple", "stratified", "cluser" or 1, 2, 3 where 1 corresponds to "simple", 2 to "stratified" and 3 to "cluster"}
	Do not set the following values unless you know what you are doing!
	\item{x}{a vector of measurements for each unit in the population. By default x is not used, and the builtin data set sscsample.data is used}
	\item{strata}{a corresponding vector for each unit in the population indicating membership to a stratum}
	\item{cluster}{a corresponding vector for each unit in the population indicating membership to a cluster}
	\item{ret}{this argument is deprecated}
	\item{print}{if false then the screen output will be supressed.}
      }
\seealso{
	\code{\link{sscsample.data}}
}
\value{
	A list will be returned with the following components:
	\item{samples}{a matrix with the number of rows equal to size and the number of columns equal to n.samples. Each column corresponds to a sample drawn from the population}
	\item{s.strata}{a matrix showing how many units from each stratum were included in the sample}
	\item{means}{a vector containing the mean of each sample drawn}
}
\author{James M. Curran, Dept. of Statistics, University of Waikato, Hamilton, New Zealand.
Janko Dietzsch, Proteomics Algorithm and Simulation,Zentrum f. Bioinformatik Tuebingen
Fakultaet f. Informations- und Kognitionswissenschaften, Universitaet Tuebingen}
\examples{
## Draw 200 samples of size 20 using simple random sampling
sscsample(20,200)

## Draw 200 samples of size 20 using simple random sampling and store the
## results. Extract the means of all 200 samples, and the 50th sample
res = sscsample(20,200)
res$means
res$samples[,50]
}
\keyword{misc}

