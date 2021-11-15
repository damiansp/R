get.confidence.interval <- function(mean, sd, n, level=0.95) {
  se <- sd / sqrt(n)
  upper.limit <- 1 - ((1 - level) / 2)
  x <- qnorm(upper.limit)
  margin <- x * se
  mean + c(-margin, margin)
}


# Test
get.confidence.interval(0, 1, 100)
get.confidence.interval(0, 0.2, 100)
get.confidence.interval(10, 0.1, 20)
get.confidence.interval(10, 0.1, 20, level=0.99)