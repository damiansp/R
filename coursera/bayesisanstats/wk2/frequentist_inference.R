getwd()
setwd('~/Learning/R/coursera/bayesisanstats')


# Bernoulli
likelihood <- function(n, y, theta) {
  theta^y * (1 - theta)^(n-y)
}


log.likelihood <- function(n, y, theta) {
  y*log(theta) + (n - y)*log(1 - theta)
}

N <- 400
Y <- 99
MLE <- Y / N
theta <- seq(0.01, 0.99, 0.01)
plot(theta, likelihood(N, Y, theta), type='l')
abline(v=MLE, col=2)

plot(theta, log.likelihood(N, Y, theta), type='l')
abline(v=MLE, col=2)