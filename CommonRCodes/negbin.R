negbin <- function(x, m, k) { # m is mean(x)
  (1 + m/k)^(-k) * (m / (m + k))^x * gamma(k + x) / (factorial(x) * gamma(k))
}