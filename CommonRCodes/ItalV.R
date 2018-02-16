italv <- function() {
	person <- sample(c("I", "you", "he", "we", "y'all", "they"), 1)
	conj <- sample(c("present", "imperfect", "passato remoto", "past", "future", "subjunctive", "subjunctive imperfect", "imperative", "conditional"), 1)
	unlist(list(person, conj))
	}