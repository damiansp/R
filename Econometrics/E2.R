#====================#
#                    #
#   Ch 2 Exercises   #
#                    #
#====================#
rm(list = ls())
load('~/Desktop/R/Econometrics/Econometrics.RData')

# 1 
neg12M = function (n) {
	#creates an nxn matrix A, with 2s along the main diag,
	# 1s at A[ii] and A[nn], -1s at A[i, i-1], and 0s
	# elsewhere 
	A = diag(c(1, rep(2, n - 2), 1))

	for (i in 2:(n - 1)) {
		A[i, i - 1] = A[i, i + 1] = -1
	}

	A[1,2] = A[n, n-1] = -1
	A
}


# 2
data(Parade2005)
head(Parade2005)

# (a) Mean earnings for CA
tapply(Parade2005$earnings, Parade2005$state, mean)['CA']

# (b) Number from sample living in ID
stateSamp = table(Parade2005$state)
stateSamp / sum(stateSamp)

# (c) Mean and Median earnings for celebrities
tapply(Parade2005$earnings, Parade2005$celebrity, mean)
tapply(Parade2005$earnings, Parade2005$celebrity, median)
truehist(Parade2005$earnings[Parade2005$celebrity == 'yes'])

# (d) Box plots of log(earnings) by celebrity
plot(log(earnings, base = 10) ~ celebrity, data = Parade2005)


# 3 
truehist(Parade2005$earnings)
lines(density(Parade2005$earnings))

truehist(log(Parade2005$earnings, base = 10), ylim = c(0, 1.4))
lines(density(log(Parade2005$earnings, base = 10)))


# 4
data(CPS1988)
head(CPS1988)

# (a) log(wage) ~ exp, ed
plot(log(wage, base = 10) ~ experience, data = CPS1988, pch = 16, 
	 col = rgb(0, 0, 0, 0.05))
with(CPS1988, lines(lowess(log(wage, base = 10) ~ experience), col = 2))
plot(log(wage, base = 10) ~ education, data = CPS1988, pch = 16, 
	 col = rgb(0, 0, 0, 0.05))
with(CPS1988, lines(lowess(log(wage, base = 10) ~ education), col = 2))

# (b) 
plot(log(wage) ~ as.factor(education), data = CPS1988)
plot(log(wage) ~ as.factor(experience), data = CPS1988)

# (d)
plot(log(wage) ~ ethnicity, data = CPS1988)
plot(log(wage) ~ smsa, data = CPS1988)
plot(log(wage) ~ region, data = CPS1988)
plot(log(wage) ~ parttime, data = CPS1988)

full = subset(CPS1988, parttime == 'no')
truehist(log(full$wage))
m = lm(log(wage) ~ (education + experience + smsa + ethnicity + region)^2,
	   data = full)
m.r = step(m, direction = 'both')
summary(m.r)



save.image('~/Desktop/R/Econometrics/Econometrics.RData')