\name{AFT.mat}
\alias{AFT.mat}
\docType{data}
\title{Matrix of Afternoon High Frequency S&P Indices}
\description{
59 x 6 matrix of the afternoon values of the 6 indicators computed for 59 trading days 
}
\details{
      range nbticks          vol   volratio  l2slope  ticksep
[1,]   4.8     220 5.186267e-08 0.07729659 320.0999 9.482759
[2,]   7.6     216 5.553202e-08 0.15074484 319.5855 7.941176
[3,]   6.8     206 6.997459e-08 0.14162258 306.5356 7.253521
[4,]   5.6     223 5.573436e-08 0.12585191 331.0890 8.173488
[5,]   6.0     202 6.540085e-08 0.05605950 306.5158 7.314424
[6,]   7.3     221 7.859222e-08 0.02101963 315.5553 7.786260
}
\source{
R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag, p. 193}
\examples{
data(AFT.mat)
## maybe str(AFT.mat) ; plot(AFT.mat) ...
}
\keyword{datasets}
