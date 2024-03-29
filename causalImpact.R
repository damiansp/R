#install.packages('CausalImpact', dependencies=T)
library(CausalImpact)


# Create Example Data Set
#set.seed(333)
x1 <- 100 + arima.sim(model=list(ar=0.999), n=100)
y <- 1.2*x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)
head(data)

matplot(data, type='l', lty=1)


# Analyze
pre <- c(1, 70)
post <- c(71, 100)
impact <- CausalImpact(data, pre, post)


# Plot
plot(impact)


# Incorporating Date/Time
times <- seq.Date(as.Date('1976-11-03'), by=1, length=100)
data <- zoo(cbind(y, x1), times)
head(data)

pre <- as.Date(c(times[1], times[70]))
post <- as.Date(c(times[71], times[100]))
impact <- CausalImpact(data, pre, post)
plot(impact)


# Printing Summary
summary(impact)


# Adjusting the Model
impact <- CausalImpact(data, pre, post, model.args=list(niter=5000, nseasons=7))
plot(impact)


# Custom Model
post.pd <- c(71, 100)
post.resp <- y[post.pd[1]:post.pd[2]]
y[post.pd[1]:post.pd[2]] <- NA

ss <- AddLocalLevel(list(), y)
bsts.mod <- bsts(y ~ x1, ss, niter=1000)
impact <- CausalImpact(bsts.model=bsts.mod, post.period.respons=post.resp)
plot(impact)

summary(impact)
summary(impact, 'report')