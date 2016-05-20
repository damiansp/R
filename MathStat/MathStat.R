rm(list=ls())

load('~/Desktop/R/MathStat/MathStat.RData')
source('~/Desktop/R/Common R Codes/g.test.R', chdir = TRUE)

# Plot the empirical cumulative distribution function:
x <- sort(runif(30, 0, 30))
plot.ecdf(x)
abline(0, 1/30, col='grey')

#

BeerWings <- read.csv('~/Desktop/R/MathStat/MathStatsData/Beerwings.csv')
tapply(BeerWings$Hotwings, BeerWings$Gender, mean)
observed <- mean(BeerWings$Hotwings[BeerWings$Gender == 'M']) - 
			mean(BeerWings$Hotwings[BeerWings$Gender == 'F'])
hotwings <- BeerWings$Hotwings # Same as:
hotwings <- subset(BeerWings, select=Hotwings, drop=T)
n <- 10^5 - 1	# Reps
result <- numeric(n)
for (i in 1:n) {
	index <- sample(30, size=15, replace=F)
	result[i] <- mean(hotwings[index]) - mean(hotwings[-index])
}

hist(result, xlab="xbar1 - xbar2", main="Permutation Distribution for hotwings", col='cyan',
	 border='orange')
abline(v=observed, col='blue')
(sum(result >= observed) + 1) / (n + 1)

	# 3.3.1 Implementation Issues
	Verizon <- read.csv('~/Desktop/R/MathStat/MathStatsData/Verizon.csv')
	tapply(Verizon$Time, Verizon$Group, mean)
	Time <- Verizon$Time
	Time.ILEC <- Verizon$Time[Verizon$Group == 'ILEC']
	Time.CLEC <- Verizon$Time[Verizon$Group == 'CLEC']
	
	(obs <- mean(Time.ILEC) - mean(Time.CLEC))
	n <- 10^4 - 1
	result <- numeric(n)
	for (i in 1:n) {
		index <- sample(1687, size=1664, replace=F) # 1687 = observed times; 1664=ILEC sample 
													# size
		result[i] <- mean(Time[index]) - mean(Time[-index])
	}
	
	hist(result, xlab='xbar1 - xbar2', main='Permutation Distribution for Verizon Times')
	abline(v=obs, lty=2, col='blue')
	(sum(result <= obs) + 1) / (n + 1)

	# 3.3.3 Other Statistics
	obs <- median(Time.ILEC) - median(Time.CLEC)	
	n <- 10^4 - 1
	res <- numeric(n)
	
	for (i in 1:n) {
		index <- sample(1687, size=1664, replace=F)
		res[i] <- median(Time[index]) - median(Time[-index])
	}
	hist(res)
	abline(v=obs, lty=2, col='blue')	
	(sum(res <= obs) + 1) / (n + 1)	# p val
	
	obs <- (mean(Time.ILEC, trim=0.25) - mean(Time.CLEC, trim=0.25))
	res <- numeric(n)
	for (i in 1:n) {
		index <- sample(1687, size=1664, replace=F)
		res[i] <- mean(Time[index], trim=0.25) - mean(Time[-index], trim=0.25)
	}
	hist(res)
	abline(v=obs, lty=2, col='blue')	
	(sum(res <= obs) + 1) / (n + 1)	# p val

# 3.4 Contingency Tables
	# 3.4.1 Permutation Test for Independence
	chisq <- function(obsM) {
		expected <- outer(rowSums(obsM), colSums(obsM)) / sum(obsM)
		sum((obsM - expected)^2 / expected)
	}
	
	GSS <- read.csv('~/Desktop/R/MathStat/MathStatsData/GSS2002.csv')
	education <- GSS$Education
	deathPenalty <- GSS$DeathPenalty
	(observed <- chisq(table(education, deathPenalty)))

	nas <- which(is.na(education) | is.na(deathPenalty))
	educ2 <- education[-nas]
	deathPenalty2 <- deathPenalty[-nas]
	
	n <- 10^4 - 1
	res <- numeric(n)
	for (i in 1:n) {
		dpPerm <- sample(deathPenalty2)
		GSS.table <- table(educ2, dpPerm)
		res[i] <- chisq(GSS.table)
	}
	
	hist(res, xlab='X^2')
	(sum(res >= observed) + 1) / (n + 1)
	
# 3.5 Chi-Squared Test of Independence
chisq.test(GSS$Education, GSS$DeathPenalty)
chisq.test(table(GSS$Education, GSS$DeathPenalty))
g.test(table(GSS$Education, GSS$DeathPenalty))

# 3.6 Test of Homogeneity
candy.mat <- rbind(c(42, 20, 38), c(33, 27, 50))
chisq.test(candy.mat)
g.test(candy.mat)
fisher.test(candy.mat)

# 3.7 Goodness of Fit: All Parameters Known
# 3.8 Goodness of Fit: Some Parameters Estimated
Phillies <- read.csv('~/Desktop/R/MathStat/MathStatsData/Phillies2009.csv')
homeruns <- subset(Phillies, select=HomeRuns, drop=T)
lambda <- mean(homeruns)
dpois(0:4, lambda) #theoretical model (i.e., % of time expected to get 0:4 hr)
table(homeruns) / 162 # empirical

#============================#
#							 #
#  4 Sampling Distributions  #
#							 #
#============================#

# 4.1 Sampling Distributions
myMeans <- numeric(1000)
for (i in 1:1000) {
	x <- rexp(100, rate = 1/15)
	myMeans[i] <- mean(x)
}

hist(myMeans)
dev.new()
qqnorm(myMeans)
qqline(myMeans)
mean(myMeans)
sd(myMeans)

myMax <- numeric(1000)
for (i in 1:1000) {
	y <- runif(12)
	myMax[i] <- max(y)
}

hist(myMax)



# 4.2 Calculating Sampling Distributions
# 4.3 The Central Limit Theorem
myMeans <- numeric(1000)
for (i in 1:1000) {
	x <- rgamma(30, shape=5, rate=2)
	myMeans[i] <- mean(x)
}
hist(rgamma(1000, 5, 2))
hist(myMeans)
qqnorm(myMeans)
qqline(myMeans)
mean(myMeans)
sd(myMeans)
mean(myMeans > 3)

	# 4.3.1 Central Limit Theorem for Binomial Data
	# to compute: choose(n, k)*(p^k)*(1-p)^(n-k)
	# dbinom(k, n, p)
	dbinom(25, 120, 0.3)
	pbinom(25, 120, 0.3)
	
	

#===================#
#		  		    #
#  5 The Bootstrap  #
#					#
#===================#

# 5.1 Introduction to the Bootstrap
# Draw a random sample of 16 ~Gamma(1, 1/2)
mySample <- rgamma(16, 1, 1/2)

# Simulate bootstrapped distribution
n <- 10^5
myBoot <- numeric(n)
for (i in 1:n) {
	x <- sample(mySample, 16, T)
	myBoot[i] <- mean(x)
}
hist(myBoot)
mean(myBoot)
sd(myBoot)


# 5.2 The Plug-In Principle
	# 5.2.1 Estimating the Population Distribution
	# 5.2.2 How Useful Is the Bootstrap Distribution?
	Bangladesh <- read.csv('~/Desktop/R/MathStat/MathStatsData/Bangladesh.csv')
	Arsenic <- Bangladesh$Arsenic
	hist(Arsenic)
	qqnorm(Arsenic); qqline(Arsenic)
	
	n <- length(Arsenic)
	N <- 10^5
	arsenicMean <- numeric(N)
	for (i in 1:N) {
		x <- sample(Arsenic, n, T)
		arsenicMean[i] <- mean(x)
	}
	hist(arsenicMean)
	abline(v=mean(Arsenic), col=2); abline(v=mean(arsenicMean), col=3)
	qqnorm(arsenicMean); qqline(arsenicMean)
	
	mean(arsenicMean)
	mean(arsenicMean) - mean(Arsenic) # bias
	sd(arsenicMean)	# bootstrap SE
	sd(Arsenic) / sqrt(n) # SE
	
# 5.3 Bootstrap Percentile Intervals
quantile(arsenicMean, probs=c(0.025, 0.975)) # bootstrapped 95%CI for mean
mean(Arsenic) + c(-1.96, 1.96)*(sd(Arsenic) / sqrt(n)) # usual 96%CI

# 5.4 Two Sample Bootstrap
TV <- read.csv('~/Desktop/R/MathStat/MathStatsData/TV.csv')
timesBasic <- subset(TV, select=Times, subset=(Cable == 'Basic'), drop=T)
timesExt <- subset(TV, select=Times, subset=(Cable == 'Extended'), drop=T)
n <- 10^5
timesDiffMean <- numeric(n)
for (i in 1:n) {
	basicSample <- sample(timesBasic, 10, T)
	extSample <- sample(timesExt, 10, T)
	timesDiffMean[i] <- mean(basicSample) - mean(extSample)
}
hist(timesDiffMean)
abline(v= mean(timesBasic) - mean(timesExt), col=2)

qqnorm(timesDiffMean); qqline(timesDiffMean)

mean(timesBasic) - mean(timesExt)
mean(timesDiffMean)
sd(timesDiffMean)
quantile(timesDiffMean, probs=c(0.025, 0.975))
mean(timesDiffMean) - mean(timesBasic) - mean(timesExt)

# 5.5 Other Statistics
timeILEC <- subset(Verizon, select=Time, Group=='ILEC', drop=T)
timeCLEC <- subset(Verizon, select=Time, Group=='CLEC', drop=T)
n <- 10^4
timeRatioMean <- numeric(n)
for (i in 1:n) {
	ILECSample <- sample(timeILEC, 1664, T)
	CLECSample <- sample(timeCLEC, 23, T)
	timeRatioMean[i] <-	mean(ILECSample) / mean(CLECSample)
}

hist(timeRatioMean)
abline(v=mean(timeILEC) / mean(timeCLEC), col='blue', lty=4)
qqnorm(timeRatioMean); qqline(timeRatioMean)

# 5.7 Monte Carlo Sampling: The "Second Bootstrap Principle"
# 5.8 Accuracy of Bootstrap Distributions
	# 5.8.1 Sample Mean: Large Sample Size
	# 5.8.1 Sample Mean: Small Sample Size
	# 5.8.3 Sample Median
	
	

#================#
#		  	     #
#  6 Estimation  #
#		  		 #
#================#

# 6.1 Maximum Likelihood Estimation
	# 6.1.1 Maximum Likelihood for Discrete Distributions
		
save.image('~/Desktop/R/MathStat/MathStat.RData')