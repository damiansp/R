##################################
# Ch 4 Basic Statistical Methods #
##################################

load('AnalyzingLinguisticData.RData')
library(languageR)
library(lattice)
library(MASS)

# 4.3 Paired Vectors
	# 4.3.2 Functional relations: linear regression
plot(ratings$meanWeightRating, ratings$meanSizeRating)

		# 4.3.2.1 Slope and intercept
abline(0.527, 0.926)

		# 4.3.2.3 Correlation
mvrnormplot.fnc(r=0.9)

	#4.3.3 What does the joint density look like?
x <- mvrnorm(n=1000, mu=c(0,0), Sigma=cbind(c(1,0.8), c(0.8,1)))
head(x); cor(x[,1], x[,2]) #should be appx = param 0.8
#note difference in cor() and cov()
cor(x[,1], x[,2]); cor(x[,1], 100*x[,2]); cor(0.01*x[,1], 100*x[,2])
cov(x[,1], x[,2]); cov(x[,1], 100*x[,2]); cov(0.01*x[,1], 100*x[,2])
persp(kde2d(x[,1], x[,2], n=40), phi=30, theta=20, d=0.75, col='cyan', shade=0.6, ltheta=-75, border=NA, xlab="X", ylab="Y", zlab='Density', main="Simulated Bivariate Normal Density\n(Covariance = 0.8, n=1000)")

n <- 10000
lambdas <- rlnorm(n, 1, 4)	#log-normal
mat <- matrix(nrow=n, ncol=2)
for(i in 1:n) {
	mat[i,] <- rpois(2, lambdas[i])
	}
mat[1:10,]
mat <- log(mat+1)

persp(kde2d(mat[,1], mat[,2], n=50), phi=30, theta=110, d=0.75, col='hotpink', shade=0.6, ltheta=0, border=NA, xlab="X", ylab="Y", zlab='Density', main="Simulated Bivariate Lognormal-Poisson Density\n(n=10 000)")


save.image('AnalyzingLinguisticData.RData')
quit()