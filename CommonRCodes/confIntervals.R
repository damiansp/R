# Calculates a given confidence interval (e.g., for a 95% CI, percent = .95) of a 
# given sample with xbar=mean, s=sd, and n
CI <- function(percent, xbar, s, n) {
  # percent = percent confidence; xbar = sample mean; s = sample sd, n = sample n
  # estimation reliability depends on size of sample:
  if(n >= 30) {
    z <- qnorm((1 + percent) / 2)
    list(xbar - ((z*s)/sqrt(n)), xbar + ((z*s)/sqrt(n)))
  } else {
    # for smaller estimates use t approximation
    t <- qt((1 + percent) / 2, df=n-1)
    list(xbar - ((t*s)/sqrt(n)), xbar + (t*s)/sqrt(n))
  }
}

# Test
CI(0.95, 8, 2, 10)