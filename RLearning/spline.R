n <- 100
x <- seq(1, 10, length=n)

f <- function(x, sd) {
  10*sin(x) - 0.4*x^0.8 - 0.3*x^1.2 + 0.8*x^2 - 4*x + 7 + rnorm(n, sd=sd)
}

y <- f(x, 4)
plot(y ~ x)
xv <- seq(1, 10, length=1000)

for (i in 1:6) {
  p <- lm(y ~ poly(x, degree=i))
  lines(xv, predict(p, newdata=data.frame(x=xv)), col=i + 1)	
}
lines(xv, f(xv, 0), lwd=2)