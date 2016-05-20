#===============================#
#                               #
#  Ch 8: Long-Memory Processes	#
#                               #
#===============================#

rm(list = ls())
load('~/Desktop/R/Time Series/TimeSeries.RData')
source('~/Desktop/SM/get.hist.quote2.R')

#options(digits = 5)
library(nlme)
library(MASS)
library(tseries)

# 8.2 Fractional Differencing
# assume a model with differencing param, d = 0.45, e.g.
# y[t] = x[t] - 0.450x[t-1] - 0.12375x[t-2]  - ... - 0.00128731x[t-40]

# Coeffs calculated as:
cf = rep(0, 40)
d = 0.45
cf[1] = -d
for (i in 1:39) {
	cf[i + 1] = -cf[i] * (d - i) / (i + 1)
}
plot(cf, type = 'l')

# For long-memory process:
# 2d - 1 = -lambda  <==> d = (1 - lambda) / 2
# In practice, fitting has to truncate FARIMA(0, d, 0) to some finite lag L, 
# equivalent to AR(L), but the FARIMA model is dependent only on the single 
# parameter, d.








save.image('~/Desktop/R/Time Series/TimeSeries.RData')