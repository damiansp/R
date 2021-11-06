#---------#---------#---------#---------#---------#---------#---------#---------
setwd('~/Learning/R/mixedModels/1_mixturedOfGaussians')


# Using mixtures of normals to simulate other types of distribution

# Bimodal
x <- seq(-5, 12, length=100)
y <- 0.6*dnorm(x, 0, 1) + 0.4*dnorm(x, 5, 2)
plot(x, y, type='l', ylab='Density', las=1)


# Skewed
y <- 0.55*dnorm(x, 0, sqrt(2)) + 0.45*dnorm(x, 3, 4)
plot(x, y, type='l', ylab='Density', las=1)


# Heavy-tailed
x <- seq(-12, 12, length=100)
y <- (0.4*dnorm(x, 0, sqrt(2)) 
      + 0.4*dnorm(x, 0, sqrt(16)) 
      + 0.2*dnorm(x, 0, sqrt(20)))
z <- dnorm(x, 0, sqrt(0.4*2 + 0.4*16 + 0.2*20))
plot(x, y, type='l', ylab='Density', las=1)
lines(x, z, col=2)
legend('topleft', lty=1, col=1:2, legend=c('Heavy-Tailed (Mixture)', 'Normal'))