rjap <- function() {
	C <- sample(c("","k","g","s","z","t","d","n","h","p","b","y","r", "end"), 1)
	V <- c("a", "i", "u", "e", "o", "")
	if(C=="n") {
		v <- sample(V, 1)
		}
	else if(C == "end"){
		v <- c("")
		}
	else {
		v <- sample(V[-6], 1)
		}
	paste(C, v, sep="")
	}