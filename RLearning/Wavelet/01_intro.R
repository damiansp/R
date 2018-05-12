haar <- function(x) {
  if (x < 0 | x >= 1) {
  	0
  } else if (x < 0.5) {
  	1
  } else {
  	-1
  }
}

x <- seq(-2, 2, length=100)
y <- numeric(100)
for (i in 1:length(x)) {
  y[i] <- haar(x[i])
}

plot(x, y, type='l', ylim=c(-2, 2))

psi.jk <- function(x, j, k) {
  2^(j/2) * haar(x*2^j - k)
}

for (j in 1:6) {
  for (k in 1:6) {
    for (i in 1:length(x)) {
    y[i] <- psi.jk(x[i], j, k)
  }
  lines(x, y, col=rgb(j/6, 0, k/6))  	
  }
}
