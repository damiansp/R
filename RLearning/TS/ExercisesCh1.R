###########################
#                         #
#  Ch 1 Time Series Data  #
#        Exercises        #
#                         #
###########################

load("TSExercises.RData")
options(digits = 3)
library(MASS)



# 1
CBE <- read.table("http://www.massey.ac.nz/~pscowper/ts/cbe.dat", header=T)
beer.ts <- ts(CBE[,2], start=1958, freq=12)
choc.ts <- ts(CBE[,1], start=1958, freq=12)
par(mfrow=c(2,1))
plot(beer.ts)
plot(choc.ts)

beer.annual <- aggregate(beer.ts, FUN=mean)
choc.annual <- aggregate(choc.ts, FUN=mean)
plot(beer.ts); lines(beer.annual, col=2)
plot(choc.ts); lines(choc.annual, col=2)

plot(decompose(beer.ts))	#$seasonal, $trend, $random
plot(decompose(choc.ts))

plot(decompose(beer.ts)$trend + decompose(beer.ts)$seasonal, col=2)
lines(decompose(beer.ts)$trend)
plot(decompose(choc.ts)$trend + decompose(choc.ts)$seasonal, col=2)
lines(decompose(choc.ts)$trend)


save.image("TSExercises.RData")
quit()