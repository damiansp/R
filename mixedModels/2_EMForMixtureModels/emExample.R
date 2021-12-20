rm(list=ls())


# Generate data from a mixture of 2 components
KK <- 2
w.true <- 0.6
mu.true <- c(0, 5)
sigma.true <- 1 # same for both comps.


# Simulate a sample from the mixure
n <- 120
cc <- sample(1:KK, n, replace=T, prob=c(w.true, 1 - w.true))
x <- numeric(n)
for (i in 1:n) {
  x[i] <- rnorm(1, mu.true[cc[i]], sigma.true)
}

par(mfrow=c(1,1))
xx.true = seq(-8,11,length=200)
yy.true = w.true*dnorm(xx.true, mu.true[1], sigma.true) + 
          (1-w.true)*dnorm(xx.true, mu.true[2], sigma.true) 
plot(xx.true, yy.true, type='l', xlab='x', ylab='True density', lwd=2)
points(x, rep(0,n), col=cc)


# EM (univariate) to estimate params
# initialize with prior guess/estimate
w <- 0.5
mu <- rnorm(KK, mean(x), sd(x)) # 2 values sampled from mean & sd of all data
sigma <- sd(x)

# Plot initial est
xx = seq(-8,11,length=200)
yy = w*dnorm(xx, mu[1], sigma) + (1-w)*dnorm(xx, mu[2], sigma)
plot(xx, yy, type='l', ylim=c(0, max(yy)), xlab='x', ylab='Initial density')
points(x, rep(0,n), col=cc)

s  = 0
sw = FALSE
QQ = -Inf
QQ.out = NULL
epsilon = 10^-5


# Checking convergence of the algorithm
while(!sw) {
  # E step
  v = array(0, dim=c(n,KK))
  v[,1] = log(w) + dnorm(x, mu[1], sigma, log=TRUE)    # Compute the log of the weights
  v[,2] = log(1-w) + dnorm(x, mu[2], sigma, log=TRUE)  # Compute the log of the weights
  for(i in 1:n) {
  	 # Go from logs to actual weights in a numerically stable manner
    v[i,] = exp(v[i,] - max(v[i,]))/sum(exp(v[i,] - max(v[i,]))) 
  }
  
  # M step
  # Weights
  w = mean(v[,1])
  mu = rep(0, KK)
  for(k in 1:KK) {
    for(i in 1:n) {
      mu[k] = mu[k] + v[i,k]*x[i]
    }
    mu[k] = mu[k]/sum(v[,k])
  }
  # Standard deviations
  sigma = 0
  for(i in 1:n) {
    for(k in 1:KK) {
      sigma = sigma + v[i,k]*(x[i] - mu[k])^2
    }
  }
  sigma = sqrt(sigma/sum(v))
  
  # Check convergence
  QQn = 0
  for(i in 1:n) {
    QQn = QQn + v[i,1]*(log(w) + dnorm(x[i], mu[1], sigma, log=TRUE)) +
                v[i,2]*(log(1-w) + dnorm(x[i], mu[2], sigma, log=TRUE))
  }
  if(abs(QQn - QQ) / abs(QQn) < epsilon) {
    sw = TRUE
  }
  QQ = QQn
  QQ.out = c(QQ.out, QQ)
  s = s + 1
  print(paste(s, QQn))
  
  # Plot current estimate over data
  layout(matrix(c(1, 2), 2, 1), widths=c(1, 1), heights=c(1.3, 3))
  par(mar=c(3.1, 4.1, 0.5, 0.5))
  plot(QQ.out[1:s], type='l', xlim=c(1, max(10, s)), las=1, ylab='Q', lwd=2)
  
  par(mar=c(5, 4, 1.5, 0.5))
  xx = seq(-8, 11, length=200)
  yy = w*dnorm(xx, mu[1], sigma) + (1 - w)*dnorm(xx, mu[2], sigma)
  plot(xx, 
       yy, 
       type='l', 
       ylim=c(0, max(c(yy, yy.true))), 
       main=paste('s =', 
                  s, 
                  '   Q =', 
                  round(QQ.out[s], 4)), 
       lwd=2, 
       col=2,  
       xlab='x', 
       ylab='Density')
  lines(xx.true, yy.true, lwd=2)
  points(x, rep(0,n), col=cc)
  legend(6, 0.22, c('Truth','Estimate'), col=1:2, lty=1)
  readline('Press <return> to continue')
}


# Plot final estimate over data
layout(matrix(c(1, 2), 2, 1), widths=c(1, 1), heights=c(1.3, 3))
par(mar=c(3.1, 4.1, 0.5, 0.5))
plot(QQ.out[1:s], type='l', xlim=c(1, max(10, s)), las=1, ylab='Q', lwd=2)

par(mar=c(5, 4, 1.5, 0.5))
xx = seq(-8, 11, length=200)
yy = w*dnorm(xx, mu[1], sigma) + (1 - w)*dnorm(xx, mu[2], sigma)
plot(xx, 
     yy, 
     type='l', 
     ylim=c(0, max(c(yy, yy.true))), 
     main=paste('s =',
                s,
                '   Q =', 
                round(QQ.out[s], 4)), 
     lwd=2, 
     col=2, 
     xlab='x', 
     ylab='Density')
lines(xx.true, yy.true, lwd=2)
points(x, rep(0, n), col=cc)
legend(6, 0.22, c('Truth','Estimate'), col=1:2, lty=1, bty='n')
