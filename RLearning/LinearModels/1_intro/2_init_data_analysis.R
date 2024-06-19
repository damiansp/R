#install.packages('faraway')
library(faraway)


add.nas <- function(pima) {
	cols <- c('diastolic', 'glucose', 'triceps', 'insulin', 'bmi')
	for (col in cols) { 
		pima[pima[col] == 0, col] <- NA
	}
	pima
}


factorize.test <- function(test) {
	test <- factor(test)
	levels(test) <- c('negative', 'positive')
	test
}


plot.diastolic <- function(d) {
	par(mfrow=c(1, 3))
	hist(d)
	plot(density(d, na.rm=T))
	plot(sort(d), pch='.')
	par(mfrow=c(1, 1))
}


plot.diabetes <- function(pima) {
	quartz()
	par(mfrow=c(1, 2))
	plot(diabetes ~ diastolic, pima)
	plot(diabetes ~ test, pima)
	par(mfrow=c(1, 1))
	quartz()
	pairs(pima)
}


# 1.2 Initial Data Analysis
initial.analysis <- function() {
	data(pima)
	print(summary(pima))  # note min of 0 for many where not possible
	pima <- add.nas(pima)
	pima$test <- factorize.test(pima$test)
	print(summary(pima))
	plot.diastolic(pima$diastolic)
	plot.diabetes(pima)
}


# 1.4 History
history <- function() {
	data(stat500)
	stat500 <- data.frame(scale(stat500))
	quartz()
	plot(final ~ midterm, stat500)
	abline(0, 1)
}


main <- function() {
	initial.analysis()
	history()
}


main()