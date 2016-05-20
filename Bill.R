t <- 0:12
months <- c(rep(c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
				  'Jul', 'Aug', 'Sep'), 1), 'Oct')
events <- c(0, 1, 0, 3, 5, 5, 6, 8, 1, 16, 25, 55, 64)
plays <- c(0, 3, 0, 11, 8, 4, 7, 12, 2, 53, 64, 177, 380)

bill <- data.frame(t, months, events, plays)
attach(bill)

plot(plays ~ t, xlim=c(0, 15), ylim=c(0, 600), xlab='Months\n(0 = Oct, 2012)', 	
	 ylab='plays/events')
points(events ~ t, col=2)

pmod.lin <- lm(plays ~ t - 1)
summary(pmod.lin)
abline(pmod.lin)

pmod.q <- lm(plays ~ t + I(t^2) - 1)
summary(pmod.q)
xv <- seq(0, 16, 0.1)
yv <- coef(pmod.q)[1]*xv + coef(pmod.q)[2]*xv^2
lines(xv, yv, lty=2)

emod.lin <- lm(events ~ t - 1)
summary(emod.lin)
abline(emod.lin, col=2)

emod.q <- lm(events ~ t + I(t^2) - 1)
summary(emod.q)
xv <- seq(0, 16, 0.1)
yv <- coef(emod.q)[1]*xv + coef(emod.q)[2]*xv^2
lines(xv, yv, col=2, lty=2)

legend('topleft', pch=1, col=1:2, legend=c('plays', 'events'), bty='n')
legend(-0.7, 570, lty=1:2, legend=c('linear', 'quadratic'), bty='n')