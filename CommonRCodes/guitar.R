guitar <- function(page, length){
	x <- sample(1:page, length)
	y <- sample(1:page, length)
	x <- rev(sort(unique(c(x, y))))
	rev(x[1:length])
	}