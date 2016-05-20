##Ch 5

##Started at p. 132
#Local Polynomial Fitting

library(KernSmooth)
plot(x=c(0,40), y=c(0,0.3), type="n", bty="l", xlab="velocity of galaxy (1000km/s)", ylab="density")
rug(gal)
lines(bkde(gal, bandwidth=dpik(gal)))
lines(locpoly(gal, bandwidth=dpik(gal)), col="red")



#Bootstrap and Permutation Methods
density(gal, n=1, from=20.833, to=20.834, width="SJ")$y	#0.1301
1 / (2*sqrt(length(gal))*0.13)	#0.4247
m <- 1000; res <- numeric(m)
for(i in 1:m) {
	res[i] <- median(sample(gal, replace=T))
	}
hist(res)
mean(res-median(gal))	#0.0500
sqrt(var(res))	#0.5352
truehist(res, h=0.1)
lines(density(res, width="SJ-dpi", n=256))
quantile(res, p=c(0.025, 0.975))	#[20.17, 22.07]
x <- seq(19.5, 22.5, length=500)
lines(x, dlogspline(x, logspline(res)), col="red")

library(boot)
(gal.boot <- boot(gal, function(x, i) median(x[i]), R=1000))
boot.ci(gal.boot, conf=c(0.90, 0.95), type=c("norm", "basic", "perc", "bca"))
plot(gal.boot)
