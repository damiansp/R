leverage <- function(x, p=1) {
  h <- 1 /length(x) + (x- mean(x))^2 / sum((x - mean(x))^2)
  infl <- (2 * p) / length(x)
  print(c(h, "Highly influential if >", infl))
}