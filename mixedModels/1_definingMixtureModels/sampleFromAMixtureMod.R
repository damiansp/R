# Generate n observation from a mixture of 3 Gaussians
sample.mixture <- function(n, ws, mus, sigs) {
  n.components <- length(ws)
  component <- sample(n.components, n, replace=T, prob=ws)
  x <- rnorm(n, mean=mus[component], sd=sigs[component])
  x
}


# Test
n <- 50
ws <- c(0.1, 0.2, 0.7)
mus <- c(0, 1, 4)
sigs <- c(1, 2, 1.4)

xv <- seq(-6, 10, length=300)
d1 <- dnorm(xv, mus[1], sigs[1])
d2 <- dnorm(xv, mus[2], sigs[2])
d3 <- dnorm(xv, mus[3], sigs[3])
mix <- ws[1]*d1 + ws[2]*d2 + ws[3]*d3
y.max <- max(c(d1, d2, d3, mix))
par(mfrow=c(3, 1))
plot(xv, d1, type='l', ylim=c(0, y.max), col=rgb(0, 0, 0, alpha=ws[1]), main='Model')
lines(xv, d2, col=rgb(0, 0, 0, alpha=ws[2]))
lines(xv, d3, col=rgb(0, 0, 0, alpha=ws[3]))
lines(xv, mix, lw=3)

samples <- sample.mixture(n, ws, mus, sigs)
hist(samples, 
     col=4, 
     xlim=range(xv), 
     ylim=c(0, y.max), 
     freq=F, 
     main=sprintf('%d Samples', n))
rug(samples)
lines(density(samples), lw=3)

samples <- sample.mixture(10*n, ws, mus, sigs)
hist(samples, 
     col=4, 
     xlim=range(xv), 
     ylim=c(0, y.max), 
     freq=F, 
     main=sprintf('%d Samples', 10*n))
rug(samples, col=rgb(0, 0, 0, alpha=0.2))
lines(density(samples), lw=3)
