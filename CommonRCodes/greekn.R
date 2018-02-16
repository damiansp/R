greekn <- function() {
	case <- sample(c("nominative"), 1)
	number <- sample(c('singular', 'plural'), 1)
	unlist(list(case, number))
	}