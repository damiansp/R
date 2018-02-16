hindiv <- function() {
	person <- sample(c('me', 'tu', 'tum', 'ap', 'vah', 'ham', 've'),1)
	conj <- sample(c('present imperfective', 'past imperfective', 'imperative'), 1)
	sex <- sample(c('m', 'f'), 1)
	unlist(list(person, sex, conj))
	}