dlogistic <- function(alpha = 0.01, rd = 1, N0 = 2, t = 15) {
	#alpha = 1/carrying capacity = 1/K = per capita detriment
	#rd = discrete growth factor (= lambda - 1)
	#N0 = initial population size
	#t = time to elapse
	N <- c(N0, numeric(t))
	for(i in 1:t){
		N[i + 1] <- {
			N[i] + rd*N[i]*(1 - alpha*N[i])
			}
		}
	N
	}