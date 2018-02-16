##Primer of Ecology Chapter 2: Density-Independent Demography; Problems

##2.1
#Transition Matrix:
A <- matrix(c(0,0.098,0,0,0,0, 0,0,0.342,0.026,0,0, 0,0,0.591,0.295,0,0, 0,0,0.050,0.774,0.145,0.016, 0,0,0.095,0.177,0.596,0.277, 1.642,0.437,0,0.194,0.362,0.489), 6)
rownames(A) <- colnames(A) <- c("ds","sdl","sm","m", "l", "f")

#Initial Population:
N <- c(0, 10, 10, 10, 10, 10)

M <- matrix(0, nrow=6, ncol=101) #Initialize growth matrix for 10yrs
rownames(M) <- rownames(A); colnames(M) <- 0:100

MP <- M #Matrix of proportional values

M[,1] <- N #set yr0 = N
MP[,1] <- N/sum(N)	

for(i in 2:101){
	M[,i] <- M[,i-1] %*% A
	MP[,i] <- M[,i] / sum(M[,i])
	}
par(mfrow=c(1,2))	
matplot(0:100, t(M), type="l", xlab="years", ylab="population size")
matplot(0:100, t(MP), type="l", xlab="years", ylab="percent of population")

#d (c next): Determining lambda: modeled as A*w == lambda*w with A as projection matrix, lambda an eigenvalue and w an eigenvector
(eigs.A <- eigen(A)) #eigenanalysis
dom.pos <- which.max(eigs.A[["values"]]) #indexes dominant eigenvalue (ok to discare imaginary portion)
L1 <- Re(eigs.A[["values"]][dom.pos]) #Re() takes only the real portion
L1 #This is lambda (= the dominant eigenvalue)

#c: determine steady-state distribution:
(w <- Re(eigs.A[["vectors"]][, dom.pos])) #w = dominant eigenvector
(ssd <- w/sum(w)) #This is the steady-state distribution; each value is the percentage that each stage will occur at in ssd

#d2: Elasticities:
(M <- eigen(t(A)))
(v <- Re(M$vectors[, which.max(Re(M$values))])) #dom eigenvector
(RV <- v/v[1]) #gives all reproductive values relative to youngest class (ds)
(vw.s <- v %*% t(w)) #dom eigenv of t(A) %*% t(dom eigen v of A) = partial deriv of lambda
(S <- vw.s/as.numeric(v %*% w)) #denominator = partial deriv of a[i,j]; S=sensitivities, i.e. contibutions to overall lambda

(elas <- (A/L1) * S) #elasticities; sensitivities weighted by transition probability
colSums(elas)




#2.2 Demographic analysis of an animal population
#b Determine steady-state distribution
A <- matrix(c(0,0.6747,0,0,0,0,0, 0,0.737,0.0468,0,0,0,0, 0,0,0.6610,0.0147,0,0,0, 0,0,0,0.6907,0.0518,0,0, 127,0,0,0,0,0.8091,0, 4,0,0,0,0,0,0.8091, 80,0,0,0,0,0,0.8089), 7)
(eigs.A <- eigen(A)) #eigenanalysis
dom.pos <- which.max(eigs.A[["values"]]) #indexes dominant 
(w <- Re(eigs.A[["vectors"]][, dom.pos])) #w = dominant eigenvector
(ssd <- w/sum(w)) #This is the steady-state distribution; each value is the percentage that each stage will occur at in ssd

#c Determine Lambda
(L1 <- Re(eigs.A[["values"]][dom.pos])) #Re() takes only the real portion; L1 is lambda (pop decreasing)

#d Determine elasticities
(M <- eigen(t(A)))
(v <- Re(M$vectors[, which.max(Re(M$values))])) #dom eigenvector
(RV <- v/v[1]) #gives all reproductive values relative to youngest class (ds)
(vw.s <- v %*% t(w)) #dom eigenv of t(A) %*% t(dom eigen v of A) = partial deriv of lambda
(S <- vw.s/as.numeric(v %*% w)) #denominator = partial deriv of a[i,j]; S=sensitivities, i.e. contibutions to overall lambda
(elas <- (A/L1) * S) #elasticities; sensitivities weighted by transition probability
colSums(elas)

#g #Graph dynamics over 10 (or 50) years
N <- c(0, 10, 10, 10, 10, 10, 10)
M <- matrix(0, nrow=7, ncol=101) #Initialize growth matrix for 10yrs
MP <- M #Matrix of proportional values

M[,1] <- N #set yr0 = N
MP[,1] <- N/sum(N)	

for(i in 2:101){
	M[,i] <- M[,i-1] %*% A
	MP[,i] <- M[,i] / sum(M[,i])
	}
par(mfrow=c(1,2))	
matplot(0:100, t(M), type="l", xlab="years", ylab="population size", xlim=c(0,20))
matplot(0:100, t(MP), type="l", xlab="years", ylab="percent of population", xlim=c(0,20))

