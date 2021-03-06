#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Rbook')



# 1. Mathematical Functions


# 1.1 Exponential Functions
# log, exp


# 1.2. Trig Functions
# sin, cos, tan, ...


# 1.3 Power Laws
# x^y


# 1.4 Polynomial Functions
x <- seq(0, 10, 0.1)
y1 <- 2 + 5*x - 0.2*x^2
y2 <- 2 + 5*x - 0.4*x^2
y3 <- 2 + 4*x - 0.6*x^2 + 0.04*x^3
y4 <- 2 + 4*x + 2*x^2 - 0.6*x^3 + 0.04*x^4

plot(y1 ~ x, type='l')
lines(y2 ~ x, col=2)
lines(y3 ~ x, col=3)
lines(y4 ~ x, col=4)

mm <- x / (2 + 5*x) # michaelis-menten
h1 <- 1 / (x - 2 + 4/x) # shallow hump
h2 <- 1 / (x^2 - 2 + 4/x) # steep hump

plot(mm ~ x, type='l', ylim=c(0, 0.5))
lines(h1 ~ x, col=2)
lines(h2 ~ x, col=4)


# 1.5 Gamma Function
t <- seq(0.2, 4, 0.01)
plot(t, gamma(t), type='l')


# 1.8 Sigmoid Functions
par(mfrow=c(2, 2))
x <- seq(0, 10, 0.1)
y <- 100 / (1 + 90*exp(-x))
plot(x, y, type='l')

y <- 20 + 100/(1 + exp(0.8*(3 - x)))
plot(x, y, type='l')

# Gompertz growth model
x <- -200:100
y <- 100 * exp(-exp(0.02 * x))
plot(x, y, type='l')

x <- 0:100
y <- 50 * exp(-5 * exp(-0.08 * x))
plot(x, y, type='l')


# 1.9 Biexponential Model
biexp <- function(a, b, c, d, x) {
  a*exp(b*x) + c*exp(d*x)
}
a <- 10
b <- -0.8
c <- 10
d <- -0.05
y <- biexp(a, b, c, d, x)
plot(x, y, type='l')

d <- d * -1
y <- biexp(a, b, c, d, x)
plot(x, y, type='l')

a <- 200
b <- 0.2
c <- -1
d <- 0.7
y <- biexp(a, b, c, d, x)
plot(x, y, type='l')

b <- 0.05
c <- 300
d <- -0.05
y <- biexp(a, b, c, d, x)
plot(x, y, type='l')



# 3 Continuous Probability Distributions
curve(pnorm(x), -3, 3)
arrows(-1, 0, -1, pnorm(-1), col=2)
arrows(-1, pnorm(-1), -3, pnorm(-1), col=4)
pnorm(-1)
curve(dnorm(x), -3, 3)


# 3.1 Normal Distribution
x <- seq(-3, 3, 0.01)
par(mfrow=c(2, 2))
plot(x, exp(-abs(x)), type='l')
plot(x, exp(-abs(x)^2), type='l')
plot(x, exp(-abs(x)^3), type='l')
plot(x, exp(-abs(x)^8), type='l')

x <- seq(-3, 3, 0.01)
z <- seq(-3, -1.25, 0.01)
p <- dnorm(z)
z <- c(z, -1.25, -3)
p <- c(p, min(p), min(p))
plot(x, dnorm(x), type='l')
polygon(z, p, col=6)


# 3.2 Central Limit Theorem
hist(10 * runif(10000))
means <- numeric(10000)
for (i in 1:10000) means[i] <- mean(10 * runif(5))
hist(means)
xv <- seq(0, 10, 0.1)
yv <- 5000 * dnorm(xv, mean=mean(means), sd=sd(means))
lines(xv, yv, col=2)


die1 <- sample(6, replace=T, 10000)
die2 <- sample(6, replace=T, 10000)
tosses <- die1 + die2
hist(tosses)


# 3.5 Comparing Data with a Normal Distribution
fishes <- read.table('data/fishes.txt', header=T)
head(fishes)
hist(fishes$mass, breaks=-0.5:16.5, col='darkgreen', main='')
lines(seq(-0.5, 16, 0.1), 
      length(fishes$mass)
      * dnorm(seq(-0.5, 16, 0.1), mean(fishes$mass), sd(fishes$mass)),
      col='limegreen',
      lwd=2)