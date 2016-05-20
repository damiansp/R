#how to deal with error and uncertainty in regression

#make some fake data
numpoints <- 30
VX <- rnorm(numpoints)
VY <- VX + rnorm(numpoints)

plot(VY ~ VX)

#make a regression object
R1 <- lm(VY ~ VX)
abline(R1, lwd=2)

#standard error of the regression
n <- length(R1$residuals) #number of points
SER <- sqrt(sum(R1$residuals^2)/n) #standard error

coef(R1) #a useful tool!
abline(coef(R1), col=2)

#should encompass 68% of cases
abline(coef(R1)[1]+SER, coef(R1)[2], col="blue")
abline(coef(R1)[1]-SER, coef(R1)[2], col="blue")

#should encompass 95% of cases
abline(coef(R1)[1]+2*SER, coef(R1)[2], col="red")
abline(coef(R1)[1]-2*SER, coef(R1)[2], col="red")

#using predict() to plot prediction and confidence bands

#make a data frame containing closely spaced 'x' values
#name the data exactly the same as the 'x' value in the regression
newX <- data.frame(VX=seq(-2.5, 2.5, .01))
str(newX)

#confidence=ci for regression line
#prediction=ci for predicted values of y
pred <- predict(R1, newX, interval="prediction"); pred[1:5, ]
conf <- predict(R1, newX, interval="confidence"); conf[1:5, ]

#matplot() means matrix plot
matplot(newX$VX, cbind(conf, pred[,-1]),
        lty=c(1,2,2,3,3), type="l", col=1, ylab="Y", xlab="X")

#replot the points
points(VX, VY)

#compare SER-based error bands
abline(coef(R1)[1]+2*SER, coef(R1)[2], col="red")
abline(coef(R1)[1]-2*SER, coef(R1)[2], col="red")