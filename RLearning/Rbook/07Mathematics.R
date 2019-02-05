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
