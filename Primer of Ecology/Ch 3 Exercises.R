##################
# Ch 3 Exercises #
##################

# 3.1 Dynamics of an Annual Plant
# (a)
# r[d] = 2
# (b)
K <- 100; alpha <- 0.01; 
# (c)
# (N[t+1] - N[t]) / N[t] = r[d]*(1 - alpha*N[t])
# (d)
N <- numeric(11)
N[1] <- 1
for(i in 2:11){
	N[i] <- 2*N[i-1]*(1 - alpha*N[i-1]) + N[i-1]
	}
plot(0:10, N, xlab="time", ylab="N", type='l')
abline(h=K, lty=2)
text(1, 105, "K = 100")



# 3.2 Dynamics of E. coli.
# (a)