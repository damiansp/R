spanv <- function() {
	person <- sample(c("I", "you", "he", 'we', "y'all", 'they'), 1)
	conj <- sample(c("present", "imperfect", "preterite", "future", "subjunctive", "subjunctive imperfect", "subjunctive future", "imperative", "conditional"), 1)
	unlist(list(person, conj))
	}