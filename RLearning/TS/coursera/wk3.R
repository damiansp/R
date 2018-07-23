#=========#=========#=========#=========#=========#=========#=========#=========
setwd('~/Learning/R/RLearning/TS/coursera')

par(mfrow=c(3, 1))
plot(arima.sim(n=150, list(order=c(0, 0, 0))), main='White noise')
plot(arima.sim(n=150, list(ma=c(0.33, 0.33, 0.33))), main='MA(3)')
plot(arima.sim(n=150, list(ma=rep(0.2, 5))), main='MA(5)')


# 3. AR(p) Processes
# AR(1) simulation
n <- 1000
phi <- c(0.2, 0.4)

Z <- rnorm(n)
#X <- numeric(n)
X <- Z[1]

for (t in 2:n) {
  X[t] <- phi[1]*X[t - 1] + Z[t]
}

par(mfrow=c(2, 1))
plot(X, type='l')
acf(X)

# AR(2)
X <- Z[1]
X[2] <- Z[2]

for (t in 3:n) {
  X[t] <- phi[1]*X[t - 1] + phi[2]*X[t - 2] + Z[t]
}

plot(X, type='l')
acf(X)

X.ts <- arima.sim(list(ar=c(0.4, -0.5)), n=1000)
plot(X.ts, type='l')
acf(X.ts)
