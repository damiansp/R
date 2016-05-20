\name{block.max}
\alias{block.max}
\title{BLOCK MAXIMA ESTIMATION}
\description{
    Divide the vector \code{data} into (possibly overlapping) blocks
    and estimate the shape parameter of the upper tail with
    by the GEV shape parameter of the sample of block maxima.
}
\usage{
block.max(data, overlap = 0, nb.blocks = NA, block.size = 100, method = "ml")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Numerical vector representing the sample observations}
  \item{overlap}{Integer giving the size of the overlap between successive blocks }
  \item{nb.blocks}{Number of blocks used}
  \item{block.size}{Integer giving the length of any given block}
  \item{method}{String which should be either "ml" or "lmom" depending upon the estimation method used.}
}
\details{
  The overlap given by the parameter \code{overlap} should be no greater than 50. The block  length given by the parameter \code{block.size} should be at least 100. The estimation method given by the parameter \code{method} should be either \code{ml} for maximum likelihood (default) or \code{lmom} for the method of moments.
}
\value{
	Returns a list comprising
  \item{n}{Length of the original data vector}
  \item{data}{ordered original data}
  \item{method}{the method used for the estimation}
  \item{nb.blocks}{Number of blocks used}
  \item{overlap}{Size of the overlap}
  \item{intervl.a}{vector of the left hand points of the intervals used in the estimation}
  \item{intervl.b}{vector of the right hand points of the intervals used in the estimation}
  \item{param.est}{esimate of the tail shape parameter}
}
\references{Rene Carmona: Heavy Tail Distributions and Financial Applications. Springer Verlag (2008)}
\author{Rene Carmona }

\keyword{Tail Shape Parameter}
\keyword{Block Maxima }
