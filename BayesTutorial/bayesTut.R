rm(list=ls())
load('~/Desktop/R/BayesTutorial/bayesTut.RData')

library(MASS)
#======#
#	   #
#  20  #
#	   #
#======#

# 20.2 Finite Distribution with Uniform Prior
n <- 6	# no. of fruits in bag
x <- 0:n	# no. of apples

len <- length(x)
prior <- rep(1 / len, len)
names(prior) <- x
cbind(prior)

y <- x / n	# likelihood of apple
cbind(prior, likelihood=y)

LP <- prior * y	# prior * likelihood
PP <- LP / sum(LP)	# posterior (= normalized prior * likelihood)
cbind(prior, likelihood=y, LP, PP)

sum(PP * (x - 1) / (n - 1))

# 20.3 Finite Distribution with Non-Uniform Prior
# We now use the posterior in prev example as our new prior
prior1 <- PP
cbind(prior1)
y1 <- (x - 1) / (n - 1) # new likelihood of apple on next draw
cbind(prior1, y1)
LP1 <- prior1 * y1
PP1 <- LP1 / sum(LP1)
cbind(prior1, y1, LP1, PP1)

# probablity of drawing a third apple (anywhere in the bag): 
sum(PP1 * (x - 2) / (n - 2))


#==================================#
#								   #
#  21 Bayesian Binomial Inference  #
#								   #
#==================================#

# 21.1 Binomial Inference with Conjugate Prior
# The survey data has smoker stats for university students.  Let y = no. of smokers, n = no. in
# survey, p = proportion of smokers in population
# Regard y as outcome of binomial experiment with size n and prob p.  
# Using uniform prior, find mean and sd of posterior of p
tbl <- table(survey$Smoke); tbl
n <- as.numeric(sum(tbl)); n
y <- n - as.numeric(tbl['Never']); y

# using uniform prior beta(1, 1) for p, the conjugate parameters of the posterior are:
a <- 1 + y; a
b <- 1 + (n - y); b

# For beta: c = a + b; mean = a / c; var = ab / (c^2(c + 1))
c <- a + b
mu <- a / c; mu
sigma <- sqrt(a*b / (c^2 * (c + 1))); sigma


# 21.2 Binomial Inference with Prior Parameters
# p = prop of smokers in general population; historical surveys indicate prior of p to be
# beta distributed with mean = 0.35 and sd = 0.025.  Find meand and sd of posterior of p.
# rearrange equations for mu and sigma above to get:
# c = mu(1 - mu) / sigma^2 -1
# a = c*mu
# b = c*(1 - mu)
tbl <- table(survey$Smoke); tbl
n <- as.numeric(sum(tbl)); n
y <- n - as.numeric(tbl['Never']); y

# Given prior estimates:
mu <- 0.35
sigma <- 0.025
c <- mu * (1 - mu) / sigma^2 - 1; c
a <- c * mu; a
b <- c * (1 - mu); b

# Now compute posterior conjugate params a1 and b1:
a1 <- a + y; a1
b1 <- b + (n - y); b1

# Find posterior mean and sd:
c1 <- a1 + b1
mu1 <- a1 / c1; mu1
sigma1 <- sqrt(a1*b1 / (c1^2 * (c1 + 1))); sigma1

save.image('~/Desktop/R/BayesTutorial/bayesTut.RData')