rcolor <- function() {
  nums <- as.character(0:9)
  hex <- c(nums, 'a', 'b', 'c', 'd', 'e', 'f')
  samp <- (sample(hex, 6, T))
  out <- c('#')
  for (i in samp) {
    out <- paste(out, i, sep='')
  }
  out
}