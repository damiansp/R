plot(SP)
rhs <- function(x, c) ifelse(x>c, x-c, 0)
x <- 0:(length(SP) - 1)
knots <- seq(0, x[length(x)], by=10) 
dm <- outer(1:length(SP), knots, rhs)
g <- lm(SP~dm)
plot(SP ~ x)
lines(x, predict(g))


randKnots <- function(y, reps=1) {
	rhs <- function(x, c) ifelse(x>c, x-c, 0)
	x <- 0:(length(y) - 1)
	maxNumberKnots <- ceiling(length(x) / 10)
	
	for(i in 1:reps) {
		thisNumberKnots <- sample(maxNumberKnots, 1)
		knots <- sample(x, thisNumberKnots)
		dm <- outer(1:length(x), knots, rhs)
		g <- lm(y~dm)
		plot(y ~ x)
		lines(x, predict(g))
		
	}
}

