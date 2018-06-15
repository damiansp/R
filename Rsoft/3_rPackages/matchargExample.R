multiply.by <- function(n, multiplier=c('two', 'three', 'four')) {
  multiplier <- match.arg(multiplier)
  if (multiplier == 'two') {
  	n * 2
  } else if (multiplier == 'three') {
  	n * 3
  } else {
  	n * 4
  }
}

multiply.by(10, 'four')
multiply.by(10, 'five')