#Random walk code
rwalk <- function(steps){
x <- y <- c(0)
for(i in 1:steps){
	var <- sample(2, 1)
	if(var==1){
		x <- c(x, x[i]+sample(c(-1,1), 1))
		y <- c(y, y[i])
		}
	if(var==2){
		x <- c(x, x[i])
		y <- c(y, y[i]+sample(c(-1,1), 1))
		}	
	}
plot(x, y, type="l")
points(x[1], y[1])
}


