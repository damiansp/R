#===============================#
#								#
#  Code from:					#
#  Bayesian Computation with R  #
#	by Jim Albert				#
#								#
#===============================#

rm(list=ls())

library(LearnBayes)
library(MASS)
load('~/Desktop/R/BayesianComputation/BayesianComputation.RData')

data(studentdata)
attach(studentdata)

#===========================#
#							#
#  1. An Introduction to R  #
#							#
#===========================#

# 1.2 Exploring a Student Dataset
	# 1.2.3 R Commands to Summarize and Graph a Single Batch
	(t <- table(Drink))
	plot(Drink)
	barplot(t, xlab='Drink', ylab='count')
	hoursOfSleep <- WakeUp - ToSleep
	summary(hoursOfSleep)
	hist(hoursOfSleep, main='')

	# 1.2.4 Commands to Compare Batches
	boxplot(hoursOfSleep ~ Gender)
	title(ylab='Hours of Sleep')
	femaleHaircut <- Haircut[Gender == 'female']
	maleHaircut <- Haircut[Gender == 'male']
	boxplot(Haircut ~ Gender)
	
	# 1.2.5 R Commands for Studying Relationships
	plot(jitter(ToSleep), jitter(hoursOfSleep))
	fit <- lm(hoursOfSleep ~ ToSleep)
	summary(fit)
	abline(fit)
	
# 1.3 Exploring the Robustness of the t Statistic
	# 1.3.2 Writing a Function to Compute the t Statistic
	#	Simulate both x and y as: 10 observations from ~N(50, 10^2)
	x <- rnorm(10, 50, 10)
	y <- rnorm(10, 50, 10)
	
	# Calculate the t statistic:
	tstat <- function(x, y) {
		m <- length(x)
		n <- length(y)
	
		#	Calculate sd of x and y pooled
		# 	not quite equal to: sd(c(x, y))
		sp <- sqrt(((m - 1) * sd(x)^2 + (n - 1) * sd(y)^2) / (m + n - 2))
		
		t <- (mean(x) - mean(y)) / (sp * sqrt(1/m + 1/n))
		return(t)
	}
	
	tstat(x, y)
	
	# 1.3.3 Programming a Monte Carlo Simulation
	#	Simulate the true value of alpha given that it is calculated under the (perhaps 
	#	erroneous) assumption of sampling from normal populations with equal distributions
	alpha <- 0.1; m <- n <- 10
	N <- 10000
	rejects <- 0
	
	for (i in 1:N) {
		x <- rnorm(m, 0, 1)
		y <- rnorm(n, 0, 1)
		t <- tstat(x, y)
		if (abs(t) > qt(1 - alpha/2, n + m - 2)) {
			rejects <- rejects + 1
		}
	}
	
	(trueSigLevel <- rejects / N)
	
	# 1.3.4 The Behavior of the True Significance Level Under Different Assumptions
	rejects <- 0
	tv <- c()
	
	for (i in 1:N) {
		x <- rnorm(m, 10, 2)
		y <- rexp(n, 0.1)
		t <- tstat(x, y)
		if (abs(t) > qt(1 - alpha/2, n + m -2)) {
			rejects <- rejects + 1
		}
		tv <- c(tv, t)
	}
	
	(trueSigLevel <- rejects / N)
	plot(density(tv), xlim=c(-5,8), ylim=c(0, 0.4), col=2)
	
	
# 1.6 Exercises
	# 1.6.1 
	names(studentdata)
	hist(Dvds)
	truehist(Dvds)
	summary(Dvds)
	table(Dvds)
	barplot(table(Dvds))
	
	# 1.6.2
	plot(Height ~ Gender)
	output <- boxplot(Height ~ Gender)
	output
	hdiff <- lm(Height ~ Gender)
	summary(hdiff)
	median(Height[Gender == 'male'], na.rm=T) - median(Height[Gender == 'female'], na.rm=T)
	
	# 1.6.3
	plot(jitter(WakeUp) ~ jitter(ToSleep))
	fit <- lm(WakeUp ~ ToSleep)
	abline(fit)
	predict(fit, newdata=data.frame(ToSleep=0))
	
	# 1.6.4
	binomialCI <- function(y, n, CI) {
		z <- qnorm(CI)
		p.hat <- y / n
		se <- sqrt(p.hat * (1 - p.hat) / n)
		return (c(p.hat - z*se, p.hat, p.hat + z*se))
	}
		


#===========================================#
#											#
#	2. Introduction to Bayesian Thinking	#
#											#
#===========================================#

# 2.3 Using a Discrete Prior
p <- seq(0.05, 0.95, 0.1)
prior <- c(2, 4, 8, 8, 4, 2, 1, 1, 1, 1)
# normalize
prior <- prior / sum(prior)
plot(p, prior, type='h', ylab='Prior Probability')

data <- c(11, 16) # 11 "hits" 16 "misses"
post <- pdisc(p, prior, data)
cbind(p, prior, post)

par(mfrow=c(2,1))
plot(p, prior, type='h', ylab='Prior Probability')
plot(p, post, type='h', ylab='Posterior Probability')


# 2.4 Using a Beta Prior
p <- seq(0, 1, length=500)
a <- 3.4
b <- 7.4
s <- 11	# successes
f <- 16 # failures
prior <- dbeta(p, a, b)
like <- dbeta(p, s + 1, f + 1)
post <- dbeta(p, a + s, b + f)
plot(p, post, type='l', ylab='Density', col='blue')
lines(p, like, col='green')
lines(p, prior, col='red')
legend('topright', legend=c('Prior', 'Likelihood', 'Posterior'), lty=1, 
	   col=c('red', 'green', 'blue'))
	   
# What is posterior probability that heavy sleepers are ≥50%: P(p ≥ 0.5 | data):
1 - pbeta(0.5, a + s, b + f)	# 6.8%

# 95% CI for heavy sleeper proportion:
qbeta(c(0.025, 0.975), a + s, b + f) # 23.5 ~ 53.9%

# Simulation approach:
ps <- rbeta(10000, a + s, b + f)
hist(ps, xlab='p Simulated', main='')
# Sim P(p ≥ 0.5)
sum(ps >= 0.5) / 10000	# 6.6 %
# Sim 95%CI
quantile(ps, c(0.025, 0.975))	# 23.2 ~ 53.8%


# 2.5 Using a Histogram Prior
midpt <- seq(0.05, 0.95, 0.1)
prior <- c(2, 4, 8, 7, 4, 2, 1, 1, 0.5, 0.5)
prior <- prior / sum(prior)
p <- seq(0, 1, length=1000)
plot(p, histprior(p, midpt, prior), type='l', ylim=c(0, 1))

like <- dbeta(p, s + 1, f + 1)
post <- like * histprior(p, midpt, prior)
lines(p, like, col='blue')
lines(p, post, col=2, ylab='Posterior Density')

post <- post / sum(post)
ps <- sample(p, replace=T, prob=post)
hist(ps, xlab='p')


# 2.6 Prediction
# using a discrete prior
p <- seq(0.05, 0.95, 0.1)
prior <- c(2, 4, 8, 7, 4, 2, 2, 1, 1, 0.5)
prior <- prior / sum(prior)
m <- 20
ys <- 0:20
pred <- pdiscp(p, prior, m, ys)
cbind(0:20, pred)

# using a beta prior
ab <- c(3.4, 7.4)
pred <- pbetap(ab, m, ys)
cbind(0:20, pred)

# by simulation
# simulate p* from g(p), and y.tilda (prediction) from binom f[binom](y.tilda | p*)
p <- rbeta(10000, 3.4, 7.4)
y <- rbinom(10000, 20, p)
freq <- table(y)
ys <- c(0:max(y))
predprob <- freq / sum(freq)
plot(ys, predprob, type='h', xlab='y', ylab='Predictive Probability')
# Summarize with a 95% CI
(dist <- cbind(ys, predprob))
covprob <- 0.95
discint(dist, covprob)


# 2.9 Exercises
	# 1. Estimating a proportion with a discrete prior
	p <- seq(0, 1, 0.125)
	prior <- c(0.001, 0.001, 0.95, 0.008, 0.008, 0.008, 0.008, 0.008, 0.008)
	s <- 6
	f <- 4	
	plot(p, prior, type='h', ylab='Prior Probability')
	
	data <- c(s, f)
	post <- pdisc(p, prior, data)
	cbind(p, prior, post)

	plot(p, prior, type='h', ylab='Prior Probability')
	lines(p + 0.01, post, type='h', col=2, ylab='Posterior Probability')
	
	# 2. Estimating a proportion with a histogram prior
	mdpt <- seq(0.05, 0.95, 0.1)
	prior <- c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
	prior <- prior / sum(prior)
	s <- sample(c(0, 1), 20, T)
	s <- sum(s)
	f <- 20 - s
	p <- seq(0, 1, length=1000)
	plot(p, histprior(p, midpt, prior), type='l', ylim=c(0, 1))
	
	like <- dbeta(p, s + 1, f + 1)
	post <- like * histprior(p, midpt, prior)
	lines(p, post, col=2)
	
	# 3. Estimating a proportion and prediction of a future sample
	prior <- beta(1, 1)
	post <- beta(23, 8)
	qbeta(c(0.05, 0.95), 23, 8)
	pbeta(0.6, 23, 8)
	sim <- rbeta(1000, 23, 8)
	y <- rbinom(1000, 10, sim)
	freq <- table(y)
	ys <- min(y):max(y)
	predprob <- freq / sum(freq)
	plot(ys, predprob, type='h')
	# Prob of 9 or 10:
	predprob['9'] + predprob['10']
	
	# 4. Contrasting predictions using two different priors
	# hist prior:
	mdpt <- seq(0, 1, 0.1)
	prior1 <- c(0, 0.5, 0.2, 0.2, 0.05, 0.05, 0, 0, 0, 0, 0)
	prior1 <- prior1 / sum(prior1)
	# beta prior
	p <- seq(0, 1, length=500)
	prior2 <- dbeta(p, 3, 12)
	
	# mean and sd - prior1
	mean1 <- sum(mdpt * prior1)	# mean = 0.195
	sd1 <- sqrt(sum(mdpt * prior1^2) - mean1^2)	# sd = 0.096
	mean2 <- 
	
detach(studentdata)


#===============================#
#								#
#	3 Single Parameter Models	#
#								#
#===============================#

# 3.2 Normal Distribution with Known Mean but Unknown Variance
data(footballscores)
attach(footballscores)
d <- favorite - underdog - spread
n <- length(d)
v <- sum(d^2)

P <- rchisq(1000, n) / v
s <- sqrt(1 / P)
hist(s)
quantile(s, probs=c(0.025, 0.5, 0.975))

detach(footballscores)

# 3.3 Estimating a Heart Transplant Mortality Rate
alpha <- 16
beta <- 15174
yobs <- 1
ex <- 66
y <- 0:10
lam <- alpha / beta
py <- dpois(y, lam * ex) * dgamma(lam, shape=alpha, rate=beta) / 
	  dgamma(lam, shape=alpha + y, rate = beta + ex)
cbind(y, round(py, 3))
# Simulate post density of lambda:
lambdaA <- rgamma(1000, shape=alpha + yobs, rate=beta + ex)
hist(lambdaA)
quantile(lambdaA, probs=c(0.025, 0.5, 0.975))

ex <- 1767
py <- dpois(y, lam * ex) * dgamma(lam, shape=alpha, rate=beta) / 
	  dgamma(lam, shape=alpha + y, rate = beta + ex)
cbind(y, round(py, 3))
lambdaB <- rgamma(1000, shape=alpha + yobs, rate=beta + ex)
hist(lambdaB)
quantile(lambdaB, probs=c(0.025, 0.5, 0.975))

# Compare posterirors given the different priors
lambda <- seq(0, max(c(lambdaA, lambdaB)), length=500)
par(mfrow=c(2, 1))
hist(lambdaA, freq=F, ylim=c(0, 1600))
lines(lambda, dgamma(lambda, alpha, beta))
hist(lambdaB, freq=F, ylim=c(0, 1600))
lines(lambda, dgamma(lambda, alpha, beta))

# 3.4 An Illustration of Bayesian Robustness
# Using a normal prior
mu <- 100
tau <- 12.16
sigma <- 15
n <- 4
se <- sigma / sqrt(n)
ybar <- c(110, 125, 140)
tau1 <- 1 / sqrt(1 / se^2 + 1 / tau^2)
mu1 <- (ybar / se^2 + mu / tau^2) * tau1^2
(summ1 <- cbind(ybar, mu1, tau1))

# Using a similar t prior, with same median and 95%ile
tscale <- 20 / qt(0.95, 2)
tscale
theta <- seq(60, 140, length=200)
plot(theta, 1 / tscale * dt((theta - mu) / tscale, 2), type='l', ylab='Prior')
lines(theta, 1 / 10 * dnorm((theta - mu) / tau), col=2)
legend('topright', legend=c('t', 'normal'), lty=1, col=1:2)

summ2 <- c()
for (i in 1:3) {
	theta <- seq(60, 180, length=500)
	like <- dnorm((theta - ybar[i]) / 7.5)
	prior <- dt((theta - mu) / tscale, 2)
	post <- prior * like
	post <- post / sum(post)
	m <- sum(theta * post)
	s <- sqrt(sum(theta^2 * post) - m^2)
	summ2 <- rbind(summ2, c(ybar[i], m, s))
}
summ2

normPost <- dnorm(theta, mu1[3], tau1)
normPost <- normPost / sum(normPost)
lines(theta, normPost, lty=2, col=2)
lines(theta, post, lty=2)

# 3.5 A Bayesian Test of the Fairness of a Coin
# Probability that coin is fair if witnessed 5/20 heads:
2 * pbinom(5, 20, 0.5)	# 0.041
n <- 20 	# total flips
y <- 5		# observed heads
a <- 10		# beta(10, 10) is centered on 0.5 (10 hits, 10 failse)
p <- 0.5	# probability being tested
m1 <- dbinom(y, n, p) * dbeta(p, a, a) / dbeta(p, a + y, a + n - y)
lambda <- dbinom(y, n, p) / (dbinom(y, n, p) + m1)
lambda		# Posterior probability that coin is fair

# Same as:
pbetat(p, 0.5, c(a, a), c(y, n - y))	# posterior as before; bf = "Bayes factor in support of										# the null hypothesis (Ch. 8)
# What if another (arbitrary) value had been chosen for beta parameter "a"?
loga <- seq(-4, 5, length=100)
a <- exp(loga)
m2 <- dbinom(y, n, p) * dbeta(p, a, a) / dbeta(p, a + y, a + n - y)
lambda <- dbinom(y, n, p) / (dbinom(y, n, p) + m2)
par(mfrow=c(2,1))
plot(loga, lambda, type='l', xlab='log(a)', ylab='P(coin is fair)'); abline(h=0.5, col=2)
plot(a, lambda, type='l', xlab='a', ylab='P(coin is fair)'); abline(h=0.5, col=2)
# Note P(fair coin) > 0.2 for all a
# The above is based on p(exactly 5 heads), update to p(≤ 5 heads), to better compare with 
# pbinom:
a <- 10
m3 <- 0
for (k in 0:y) {
	m3 <- m3 + dbinom(k, n, p) * dbeta(p, a, a) / dbeta(p, a + k, a + n - k)
}
lambda <- pbinom(y, n, p) / (pbinom(y, n, p) + m3)
lambda



#===============================#
#								#
#	4. Multiparameter Models	#
#								#
#===============================#

# 4.2 Normal Data with Both Parameters Unknown
data(marathontimes) 
attach(marathontimes)
mycontour(normchi2post, limits=c(220, 330, 500, 9000), time)
title(xlab='mean', ylab='variance')
S <- sum((time - mean(time))^2)
n <- length(time)
sigma2 <- S / rchisq(1000, n - 1)
mu <- rnorm(1000, mean=mean(time), sd=sqrt(sigma2) / sqrt(n))
points(mu, sigma2, col='darkgrey')
quantile(mu, probs=c(0.025, 0.5, 0.975))
quantile(sqrt(sigma2), probs=c(0.025, 0.5, 0.975))

detach(marathontimes)


# 4.3 Multinomial Model
alpha <- c(728, 584, 138)	# No. of voters for candidates a, b, c = 727, 583, 137
theta <- rdirichlet(1000, alpha)
truehist(theta[, 1])	# posterior probability of being for candidate a

# 4.4 Bioassay Experiment
x <- c(-0.86, -0.3, -0.05, 0.73)	# log(dose)
n <- c(5, 5, 5, 5)					# sample size each
y <- c(0, 1, 3, 5)					# deaths each
data <- cbind(x, n, y)
response <- cbind(y, n - y)
results <- glm(response ~ x, family=binomial)
summary(results)
plot(y ~ x)

mycontour(logisticpost, c(-4, 8, -5, 39), data)
title(xlab='beta0', ylab='beta1')
s <- simcontour(logisticpost, c(-4, 8, -5, 39), data, 10000)
points(s$x, s$y, col='darkgrey')
plot(density(s$y), xlab='beta1')
theta <- -s$x / s$y	# LD-50
hist(theta, xlab='LD-50')
quantile(theta, probs=c(0.025, 0.5, 0.975))

# 4.5 Comparing Two Proportions
# Is p equal in both samples?
sigma <- c(2, 1, 0.5, 0.25)
plo <- 0.0001
phi <- 0.9999
par(mfrow=c(2, 2))
for (i in 1:4) {
	mycontour(howardprior, c(plo, phi, plo, phi), c(1, 1, 1, 1, sigma[i]))
	title(main=paste('sigma = ', sigma[i]), xlab='p1', ylab='p2')
}

for (i in 1:4) {
	# Assume a data set where sample 1 has 3 successes and 15 failures, and s2 has 7 and 5
	mycontour(howardprior, c(plo, phi, plo, phi), c(1 + 3, 1 + 15, 1 + 7, 1 + 5, sigma[i]))
	lines(c(0, 1), c(0, 1))
	title(main=paste('sigma = ', sigma[i]), xlab='p1', ylab='p2')
}

s <- simcontour(howardprior, c(plo, phi, plo, phi), c(1 + 3, 1 + 15, 1 + 7, 1 + 5, 2), 1000)
sum(s$x > s$y) / length(s$x) # 0.011
# Thus if sigma = 2, there is a 1.1% chance that p1 > p2




#===============================================#
#												#
#	5. Introduction to Bayesian Computation		#
#												#
#===============================================#

# 5.3 Setting Up a Problem in R
myLogPost <- function(theta, data) {
	n <- length(data)
	mu <- theta[, 1]
	sigma <- exp(theta[, 2])
	val <- 0 * mu
	for (i in 1:n) {
		val <- val + dnorm(data[i], mean=mu, sd=sigma, log=T)
	}
	
	val <- val + dnorm(mu, 10, 20, log=T)
	return (val)
}

# 5.4 A Beta-Binomial Model for Overdispersion


save.image('~/Desktop/R/BayesianComputation/BayesianComputation.RData')