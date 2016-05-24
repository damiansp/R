#===============================#
#                               #
#  Fun with the Mandelbrot Set  #
#                               #
#===============================#

# Translate a complex number z -> z^2 + C
mbTrans <- function(z, C) {
  return (z^2 + C)
}

C <- 6 + 5i
z0 <- 0
z1 <- mbTrans(z0, C)
z2 <- mbTrans(z1, C)
z3 <- mbTrans(z2, C)
z4 <- mbTrans(z3, C)
z5 <- mbTrans(z4, C)

#plot(c(z0, z1, z2, z3, z4, z5))

# Create a grid of coordinates over specified ranges
gridInit <- function(rMin, rMax, iMin, iMax) {
  dr <- rMax - rMin
  di <- iMax - iMin
  plane <- matrix(nrow=di, ncol=dr)
	
  for (i in 1:di) {
    for (r in 1:dr) {
      plane[i, r] <- complex(real = rMin + (r - 1), imaginary = (iMin + (i - 1)))
    }
  }
	
  plane
}

g5x5 <- gridInit(-100, 100, -100, 100)

imageable <- function(M) {
  Mr <- Re(M)
  Mi <- Im(M)
  Md <- sqrt(Mr^2 + Mi^2)
  Md
}

image(imageable(g5x5), col = heat.colors(100))

g1 <- mbTrans(g5x5, C = (0.2 - 0.01i))
image(imageable(g1), col = heat.colors(100))
g2 <- mbTrans(g1, C = (1 - 1i))
image(imageable(g2), col = heat.colors(100))
