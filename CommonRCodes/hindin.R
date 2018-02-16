hindin <- function() {
	number <- sample(c('singular', 'plural'))
	case <- sample(c('nominative', 'oblique'))
	unlist(list(number, case))
	}