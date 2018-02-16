##Ch 3 PROBABILITY DISTRIBUTIONS

##doc began at p. 55:
#3.2 Discrete Distributions
(havelaar.tab <- xtabs(~havelaar$Frequency))
havelaar.probs <- xtabs(~havelaar$Frequency) / nrow(havelaar)
round(havelaar.probs, 3)
plot(as.numeric(names(havelaar.probs)), havelaar.probs, type="h", xlab="counts", ylab="relative freq")
mtext("observed", 3, 1)
n <- 1000
(p <- mean(havelaar$Freq/n))
counts <- 0:36
points(counts, dbinom(counts, n, p), type="l", lty=2)
points(counts, dpois(counts, lambda=n*p), type="l", lty=3)
points(counts, dnorm(counts, n*p, sd=sd(havelaar$Freq)), type="l", lty=4)
legend(22, 0.1, c("Binomial", "Poisson", "Normal"), lty=c(2, 3, 4))

#3.3 Continuous Distributions
#3.3.1 The Normal Distribution
x <- seq(-4, 4, 0.1)
plot(x, dnorm(x), type="l")
?shadenormal.fnc
shadenormal.fnc(c(0.1, 0.9))

#3.3.2 The t, F, and Chi^2 Distributions