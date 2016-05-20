#===========================#
#							#
#	Statistical Inference	#
#		Coursera			#
#							#
#===========================#

rm(list=ls())
library(UsingR)
data(father.son)
data(sleep)

# Bernoulli and Binomial
n <- 8
pvals <- seq(0, 1, length=1000)
plot(c(0, 1), c(0, 1.2), type='n', frame=F, xlab='p', ylab='likelihood')
text((0:n) / n, 1.1, as.character(0:n))
sapply( 0:n, 
	    function(x) {
		    phat <- x / n
		    if (x == 0) { lines(pvals, ((1 - pvals) / (1 - phat))^(n - x)) }
		    else if (x == n) { lines(pvals, (pvals / phat)^x, col=2) }
		    else { lines(pvals, (pvals / phat)^x * ((1 - pvals) / (1 - phat))^(n - x), col=3)}
		 } )
title(paste('Lkelihoods for n = ', n))



# Gaussian
zvals <- seq(-4, 4, length=1000)
plot(zvals, dnorm(zvals), type='l', frame=F, xlab='Z', ylab='Density')
sapply(-3:3, function(k) { abline(v=k, col='grey') } ); abline(h=0, col='darkgrey')


# Poisson
ppois(3, lambda = 2.5 * 4)	# probability of seeing ≤3 people at site in 4 hrs when avg. rate is 							# 2.5 per hour
# approximation to binomial
pbinom(2, size=500, prob=0.01)
ppois(2, lambda=500 * 0.01)

pbinom(2, size=1000, prob=0.01)
ppois(2, lambda=1000 * 0.01)



# Limits and the Law of Large Numbers
n <- 10000
means <- cumsum(rnorm(n)) / (1:n)
plot(means, type='l', ylab='Cumulative Mean', xlab='Sample Size')
abline(h=0, col='grey')



# Asymptotic Confidence Intervals
x <- father.son$sheight
# 95%CI:
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x) / sqrt(length(x))) / 12 # /12 converts in to feet

# Poisson CI for lambda
x <- 5
t <- 95.32 # 5 occurences in 95.32 days (e.g.)
lambda <- x / t
round(lambda + c(-1, 1) * qnorm(0.975) * sqrt(lambda / t), 3)

# better CI:
poisson.test(x, T=94.32)$conf

# better still:
exp(confint(glm(x ~ 1 + offset(log(t)), family=poisson(link=log))))


# Chi-Squared Distribution
x <- father.son$sheight
s <- sd(x)
n <- length(x)
# Quick and dirty CI (assumes normality)
round(sqrt((n - 1) * s^2 / qchisq(c(0.975, 0.025), n - 1)), 3)

# t Distribution and t Intervals
head(sleep)
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
diff <- g2 - g1
hist(diff)
m <- mean(diff)
s <- sd(diff)
n <- 10
# 95% CI
m + c(-1, 1) * qt(0.975, n - 1) * s / sqrt(n)
# same as:
t.test(diff) #$conf.int