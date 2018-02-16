source("f:/D830 backups Fall 2008/Robertson/courses/Data Analysis/Data Analysis 2009/lectures/air pollution.dat")
str(airpoll)
rownames(airpoll)

soMort <- data.frame(airpoll$SO2, airpoll$Mortality)
colnames(soMort) <- c("SO2", "mortal")

#just a scatterplot
plot(soMort$SO2, soMort$mortal, pch=19, col=2)
abline(lm(soMort$mortal ~ soMort$SO2))

#transform independent variable
plot(log(soMort$SO2), soMort$mortal, pch=19, col="blue")
abline(lm(soMort$mortal ~ log(soMort$SO2)))
abline(line(log(soMort$SO2), soMort$mortal), col=2)

#convex hull trimming
hull <- chull(log(soMort$SO2), soMort$mortal)
hull
plot(log(soMort$SO2), soMort$mortal, pch=19, col="blue")
polygon(log(soMort$SO2)[hull], soMort$mortal[hull], density=0, angle=30, col=2)
abline(lm(soMort$mortal~log(soMort$SO2)))
#examine the regression without the hull
abline(lm(soMort$mortal[-hull]~log(soMort$SO2)[-hull]), col=2)

#residuals and studentized residuals
library(MASS)
lm1 <- lm(soMort$mortal ~ log(soMort$SO2))
sr <- studres(lm1)
plot(lm1$residuals)
plot(sr)
plot(lm1$residuals, sr)
#plot standard deviation thresholds
abline(h=c(-2, -1, 1, 2), lty=2)

#studentized residuals as a bubble plot
plot(log(soMort$SO2), soMort$mortal)
symbols(log(soMort$SO2), soMort$mortal, circles=abs(sr), inches=.4)
polygon(log(soMort$SO2)[hull], soMort$mortal[hull], density=0, angle=30, col=2)

#second iteration of convex hull trimming??
#plot(log(soMort$SO2)[-hull], soMort$mortal[-hull], pch=19, col="blue")
lSO2 <- log(soMort$SO2)[-hull]
Mort <- soMort$mortal[-hull]
hull2 <- chull(lSO2, Mort)
#plot(lSO2, Mort)
#points(lSO2, Mort, type="n")
polygon(lSO2[hull2], Mort[hull2], density=0, angle=30, col=2)

#example of cook's distance (leverage)
lm1 <- lm(soMort$mortal ~ log(soMort$SO2))
cd1 <- cooks.distance(lm1)
plot(cd1)
abline(h=4/(60-2)) #4/n - k (coefficients, including the intercept)
#identifying individual points
identify(cd1, cex=.5) #just by index number
identify(cd1, labels=rownames(airpoll), cex=.75) #by city and state

#cook's distance as a bubble plot
plot(log(soMort$SO2), soMort$mortal)
symbols(log(soMort$SO2), soMort$mortal, circles=sqrt(cd1), inches=.4)
polygon(log(soMort$SO2)[hull], soMort$mortal[hull], density=0, angle=30, col=2)
identify(log(soMort$SO2), soMort$mortal, labels=rownames(airpoll), cex=.75) #by city and state

#hat values
plot(hatvalues(lm1))
  sum(hatvalues(lm1))
  mean(hatvalues(lm1))
  2/60
abline(h=c(2, 3)*2/60, lty=2)
identify(hatvalues(lm1), labels=rownames(airpoll), cex=.5)

#combined plot of hat values, studentized residuals, and Cook's distance
lm1 <- lm(soMort$mortal ~ log(soMort$SO2))
symbols(hatvalues(lm1), rstudent(lm1), circles=sqrt(cooks.distance(lm1)), inches=.2)
abline(v=c(2, 3)*2/60, lty=2) #average hat value, x 2&3
abline(h=c(-2, 2), lty=2)
identify(hatvalues(lm1), rstudent(lm1), labels=abbreviate(rownames(airpoll), 4))


