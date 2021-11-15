# Coefficient of variation
cv <- function(x) {
  coef.var <- sd(x) / mean(x)
  coef.var
}