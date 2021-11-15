# Convolution to blur an image
blur <- function(M, distance) {
  rows <- dim(M)[1]
  cols <- dim(M)[2]
  # Create empty matrix the size of M to write new values to
  N <- matrix(nrow=rows, ncol=cols)	
  for (i in 1:rows) {
    # get range of possible row vals
    row.min <- i - distance
    if (row.min < 1) { row.min <- 1 }
    row.max <- i + distance
    if (row.max > rows) { row.max <- rows }
    for (j in 1:cols) {
      # get range of possible col vals
      col.min <- j - distance
      if (col.min < 1) { col.min <- 1 }
      col.max <- j + distance
      if (col.max > cols) { col.max <- cols}
      N[i, j] <- mean(M[row.min:row.max, col.min:col.max])
    }
  }
  N
}


median.filter <- function(M, distance) {
  rows <- dim(M)[1]
  cols <- dim(M)[2]
  # Create empty matrix the size of M to write new values to
  N <- matrix(nrow=rows, ncol=cols)
  for (i in 1:rows) {
    # get range of possible row vals
    row.min <- i - distance
    if (row.min < 1) { row.min <- 1 }
    row.max <- i + distance
    if (row.max > rows) { row.max <- rows }
    for (j in 1:cols) {
      # get range of possible col vals
      col.min <- j - distance
      if (col.min < 1) { col.min <- 1 }
      col.max <- j + distance
      if (col.max > cols) { col.max <- cols}
      N[i, j] <- median(M[row.min:row.max, col.min:col.max])
    }
  }
  N
}


# Test
d <- 40
M <- matrix(rpois(d * d, 3), ncol=d)
par(mfrow=c(2, 3))
par(mar=c(0, 0, 0, 1))
image(M)
image(blur(M, distance=2))
image(median.filter(M, distance=2))
image(M)
image(blur(blur(M, 2), 2))
image(median.filter(median.filter(M, 2), 2))
