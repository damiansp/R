#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/TS/coursera')

library(astsa)

# 1. Visualizing and Describing TS: Time Plots, Autocovariance & Autocorrelation
# 1.2 Time plots
class(jj)
plot(jj, 
     type='o', 
     main='Johnson & Johnson quarterly earnings per share', 
     xlab='Year', 
     ylab='Earnings')
class(flu)
plot(flu, 
     main='Monthly Pneumonia and Flu Deaths (US)', 
     xlab='Months', 
     ylab='Deaths per 10K')
class(globtemp)
plot(globtemp, 
     main='Global mean land-ocean deviations from mean temp 1951-80', 
     xlab='Year',
     ylab='Temperature Deviation')
class(globtempl)
plot(globtempl,
     main='Global mean land deviations from mean temp 1951-80', 
     xlab='Year',
     ylab='Temperature Deviation')
class(star)
plot(star,
     xlab='Days',
     ylab='Magnitude',
     main='Magnitude of star at midnight form 600 consecutive days')
     
# 1.3 First Intuitions on (Weak) Stationarity
# - no trend, constant variance, no periodicity

# 1.6 Autocorrelation Function



# 2 Random Walks and Intro to Moving Averages
# 2.1 Random Walk
n <- 1000
x <- numeric(n)
x[1] <- 0
for (i in 2:n) {
  x[i] <- x[i - 1] + rnorm(1)
}

plot(x, type='l')
acf(x)
plot(diff(x), type='l')
acf(diff(x))

# 2.3 Simulating MA(2) Process
n <- 10000
noise <- rnorm(n)
ma.2 <- numeric(n)
ma.2[1:2] <- noise[1:2]
for (i in 3:n) {
  ma.2[i] <- noise[i] + 0.7*noise[i - 1] + 0.2*noise[i - 2]
}

ma.2 <- ts(ma.2[3:n])
par(mfrow=c(2, 1))
plot(ma.2, type='l')
acf(ma.2)

