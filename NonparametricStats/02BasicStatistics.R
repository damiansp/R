#===============================================#
#												#
#	Nonparametric Statistical Methods Using R	#
#		Kloke & McKean (2015)					#
#												#
#							#===================#
#							#
#	Ch 2: Basic Statistics	#
#							#
#===========================#
rm(list=ls())
load('~/Desktop/R/NonparametricStats/NPS.RData')

library(boot)
library(npsm)

# 2.1 Intro



# 2.2 Sign Test
# Is x more... than y?
# Suppose we compare ice cream brands A & B; 10 participants prefer A, 1 prefers B.  Under the null hypothesis that each brand is equally likeable, the probablity of this distribution is:
2 * dbinom(10, 11, 0.5)	# (successes, n, p), 2* b/c it is a two sided p value



# 2.3 Signed-Rank Wilcoxon
	# 2.3.1 Estimation and Confidence Intervals
	
	# 2.3.2 Computation in R
	x <- rnorm(100)
	wilcox.test(x, conf.int=T)
	t.test(x, conf.int=T)
	
	x2 <- runif(1000, -0.5, 0.5)
	wilcox.test(x2, conf.int=T)
	t.test(x2, conf.int=T)
	
	wilcox.test(x, x2, conf.int=T)
	t.test(x, x2, conf.int=T)
	
	# Ex. 2.3.1 
	school <- c(82, 69, 73, 43, 58, 56, 76, 65)
	home   <- c(63, 42, 74, 37, 51, 43, 80, 62)
	response <- school - home
	wilcox.test(response, alternative = 'greater', conf.int = T)
	t.test(response, alternative = 'greater', conf.int = T)
	
	# Ex. 2.3.2 Power of Wilcoxon and t Tests
	n <- 30
	df <- 2
	nsims <- 10000
	mu <- 0.5
	collwil <- rep(0, nsims)
	collt <- rep(0, nsims)
	for (i in 1:nsims) {
		x <- rt(n, df) + mu
		wil <- wilcox.test(x)
		collwil[i] <- wil$p.value
		ttest <- t.test(x)
		collt[i] <- ttest$p.valu
	}
	
	powwil <- rep(0, nsims)
	powwil[collwil <= 0.05] <- 1
	powerwil <- sum(powwil) / nsims
	
	powt <- rep(0, nsims)
	powt[collt <= 0.05] <- 1
	powert <- sum(powt) / nsims
	
	cbind(powerwil, powert)



# 2.4 Bootstrap
# Ex. 2.4.1
x <- rnorm(25, 300, 5)	
B <- 10000	# no. of bootstaps
xbar <- rep(0, B)
for (i in 1:B) {
	xbs <- sample(x, length(x), T)
	xbar[i] <- mean(xbs)
}

se.xbar <- sd(xbar)
se.xbar

par(mfrow = c(2, 1))
hist(x)
abline(v = mean(x), col=2)
mean(x)	# 301

hist(xbar)
abline(v = median(xbar), col=2);
abline(v = quantile(xbar, prob = c(0.05, 0.95)), col = 2, lty = 2)
median(xbar)	# 301
quantile(xbar, prob = c(0.05, 0.95))	# [300, 303]

tcv <- qt(0.975, length(x) - 1)
mean(x) + c(-1, 1) * tcv * se.xbar		# [299, 303]
mean(x) + c(-1, 1) * tcv * sd(x) / sqrt(length(x))	# [299, 303]

	# 2.4.1 Percentile Bootstrap Confidence Intervals
	quantile(xbar, prob = c(0.025, 0.975))	# [299.30, 302.93]
	m <- 0.025 * 1000
	sort(xbar)[c(m, B - m)]					# [298.43, 303.62]
	
	# Use the boot library to do the same as above
	bsxbar <- boot(x, function(x, indices) { mean(x[indices]) }, B)
	boot.ci(bsxbar)
	
	
	# 2.4.2 Bootstrap Tests of Hypotheses
	# Ex. 2.4.2
	d <- school - home 	# same as the 'response' variable in Ex. 2.3.1 above
	dpm <- c(-d, d)
	n <- length(d)
	B <- 5000
	dbs <- matrix(sample(dpm, n * B, T), ncol=n)
	wilcox.teststat <- function(x) { wilcox.test(x)$statistic }
	bs.teststat <- apply(dbs, 1, wilcox.teststat)
	mean(bs.teststat >= wilcox.teststat(d))	# bootstrapped p value
	
	# Ex. 2.4.3 Bootstrap test for sample mean; sample ~N(1.5, 1)
	# Hypothesis H[0]: theta = 1 vs H[1]: theta > 1
	n <- 25
	x <- rnorm(n, 1.5, 1)
	theta.hat <- mean(x)
	x0 <- x - theta.hat + 1	# theta0 is 1
	mean(x0)				# H[0] is true
	B <- 50000
	xbar <- rep(0, B)
	for (i in 1:B) {
		xbs <- sample(x0, length(x), T)
		xbar[i] <- mean(xbs)
	}
	mean(xbar >= theta.hat)	# bootstrapped p value



# 2.5 Robustness
x <- c(1.85, 2.35, -3.85, -5.25, -0.15, 2.15, 0.15, -0.25, -0.55, 2.65)
mean(x)
median(x)

sensitivity <- function(theta.plus, theta, n) {
	(theta.plus - theta) / (1 / (n + 1))
}

outlier.x <- seq(-20, 20, length=100)

mean.sensitivity <- rep(0, 100)
median.sensitivity <- rep(0, 100)
mean.x <- mean(x)
median.x <- median(x)

for (i in 1:100) {
	mean.sensitivity[i] <- sensitivity( mean(c(x, outlier.x[i])), mean.x, 
										length(x) )
	median.sensitivity[i] <- sensitivity( median(c(x, outlier.x[i])), 
										  median.x, length(x) )
}

plot(mean.sensitivity, type='l', xlab='outlier value', ylab='sensitivity')
lines(median.sensitivity, col=2)



# 2.6 One- and Two-Sample Proportion Problems
	# 2.6.1 One-Sample Problems
	# Ex. 2.6.1 Compute a binomial p and CI estimate from a sample
	p.hat <- 10 / 143	# 10 of 143 hip replacements were squeaky
	zcv <- qnorm(0.975)
	p.hat + c(-1, 1) * zcv * sqrt(p.hat * (1 - p.hat) / 143)
	prop.test(10, 143, p=10/143, correct=F) # basically the same
	# e.g., w/ 95% conf. the true prop lies on [0.028, 0.112]
	# If the null hypothesis is that p.hat = p0 find |z|:
	# z = (p.hat - p0) / sqrt( (p0 * (1 - p0) ) / n)
	
	# Ex. 2.6.2 
	# Is the prop of lefties in prof baseball = prop in whole population?
	ind <- with(baseball, throw=='L')
	prop.test(sum(ind), length(ind), p=0.15, correct=F)
	# for prof baseball players true p is on [0.161, 0.378] (95%CI)
	binom.test(sum(ind), length(ind), p=0.15)	# conservative CI
	
	# 2.6.2 Two-Sample Problems
	# With two samples, are the prob of successes equal?
	# Ex. 2.6.3 Polio Vaccine
	# Group			n		n.polio	p.hat
	# Vaccinated	200745	57		0.000284
	# Placebo		201229	199		0.000989
	prop.test(c(57, 199), c(200745, 201229), correct=F)
	# the est differences in props is on [-0.00086, -0.00055]; p << 0.001
	
	

# 2.7 Chi^2 Tests
	# 2.7.1 Goodness-of-Fit Tests for a Single Discrete Random Variable
	# Suppose a die is rolled 370 times, wit each value coming up with
	# the following frequencies:
	x <- c(58, 55, 62, 68, 66, 61)	# Is the die fair?
	chifit <- chisq.test(x)
	chifit	# seems fair
	chifit$expected # = sum(x) / length(x) = 370 / 6
	chifit$resid^2
	
	# Ex. 2.7.1 Birth rates of Males to Swedish Ministers
	# No. of Males of 1st 7 Children:
	#	0	1	2	3	4	5	6	7
	# No. of Ministers having that no. of males:
	#	6	57	206	362	365	256	69	13
	# e.g., For 6 ministers, none of their first 7 children were male
	sons <- 0:7
	freqs <- c(6, 57, 206, 362, 365, 256, 69, 13)
	nMin <- sum(freqs)
	nChildren <- 7 * nMin
	nSons <- sum(freqs * sons)
	# The null hypothesis for p(male) for a single son is
	p.hat <- nSons / nChildren	# 0.514
	exp <- nChildren * choose(7, sons) * p.hat^sons * (1 - p.hat)^(7 - sons)
	# = nChildren * dbinom(sons, 7, p.hat)
	pmf <- dbinom(sons, 7, p.hat)	# prob mass func
	rbind(sons, round(pmf, 3))	# Expected proportions
	expected <- round(nMin * pmf, 3)	# Expected frequencies
	rbind(sons, freqs, expected)
	(test.result <- chisq.test(freqs, p=pmf))
	pchisq(test.result$statistic, df=6, lower.tail=F)
	
	# Confidence Intervals
	# Ex. 2.7.2 Ministers' Sons Continued
	p0 <- freqs[1] / nMin	# % of ministers having 0 sons
	p7 <- freqs[8] / nMin	# " ""    ""       ""   7  ""
	se <- sqrt( (p0 + p7 - (p0 - p7)^2) / n )
	lb <- p0 - p7 + qnorm(0.025) * se
	ub <- p0 - p7 + qnorm(0.975) * se
	(res <- c(p0 - p7, lb, ub))	# i.e. the difference in the proportions of 
								# ministers having 0 and 1 sons is 
								# [-0.052, 0.041] (95% CI); hence not sigdiff
	
	# 2.7.2 Several Discrete Random Variables
	# Ex. 2.7.3 Crime and Alcohol
	crime <- 	 c('arson', 'rape', 'violence', 'theft', 'coining', 'fraud')
	alcoholic <- c(50, 		88, 	155, 		379, 	 18, 		63)
	nonAlc <- 	 c(43, 		62, 	110, 		300, 	 14, 		144)
	conTable <- cbind(alcoholic, nonAlc)
	rownames(conTable) <- crime
	conTable
	chifit <- chisq.test(conTable)
	chifit
	(chifit$residuals)^2 # fraud is clearly driving significance
	conTable2 <- conTable[-6, ]
	chisq.test(conTable2) # nothing else is sig.
	
	# 2.7.3	Independence of Two Discrete Random Variables
	
	# 2.7.4 McNemar's Test
	# Suppose political candidates A and B, with known preferences for each 
	# candidate both before and after a debate; test whether there is a sig.
	# change in proportions of each
	# Ex. 2.7.4 Hodgkin's Disease and Tonsillectomy
	# Do tonsils protect against Hodgkin's disease?
	#											Sibling
	#								Tonsillectomy	No Tonsil
	# hodgkin's | Tonsillectomy 	26				15
	# patients  | No Tonsillect.	7				37
	testStat <- (15 - 7)^2 / (15 + 7)
	p <- (1 - pchisq(testStat, 1)) / 2
	pExact <- 1 - pbinom(14, (15 + 7), 0.5)
	c(testStat, p, pExact)
	
	
	
	
	
	
	

save.image('~/Desktop/R/NonparametricStats/NPS.RData')