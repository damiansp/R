chords <- function(number) {
  x <- sample(12, number, T)
  y <- sample(10, number, T)
  (cbind(x,y))
}