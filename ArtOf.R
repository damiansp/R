#==============================#
#                              #
#  "The Art of R Programming"  #
#                              #
#==============================#

rm(list=ls())
library(lattice)

# 8: Doing Math and Simulations in R

# Function minima/maxima
sqSin <- function(x) {
	y <- x^2 - sin(x)
	plot(y ~ x, type='l')
}

sqSin(seq(-30, 30, 0.1))

#(solved iteratively by gradient descent (?))
nlm(function(x) return(x^2-sin(x)), 8) # 8 = initial guess

# Derivatives 
D(expression(exp(x^2)), 'x')	# derivative with respect to x
xv <- seq(-1, 1, 0.01)
yv <- exp(xv^2)
dv <- exp(xv^2) * (2*xv)

plot(yv ~ xv, type='l', ylim=range(dv))
lines(dv ~ xv, lty=4)
abline(h=0, col=2); abline(v=0, col=2)

# Integrals
integrate(function(x) x^2, 0, 1)
xv <- seq(0, 1, 0.01)
plot(1, 1, type='n', xlim=c(0,1), ylim=c(0,1))
polygon(c(xv, 1), c(xv^2, 0), col='steelblue', border=NA)
lines(xv^2 ~ xv, type='l', lwd=2)

# Linear Algebra Operations on Vectors and Matrices
# dot product, AKA inner product:
a <- 1:3
b <- c(5, 12, 13)
crossprod(a) # = a • a # NOTE this is NOT the vector cross product, a x a
crossprod(a, b)

# Solving systems of equations
# Given: 	x1 + x2 = 2
#			-x1 + x2 = 4
# Solve for x1, x2:
# Same as:
# (| 1  1 |  (| x1 | = (| 2 |     = Ax = b
#  |-1  1 |)  | x2 |)   | 4 |)
A <- matrix(c(1,-1,1,1), 2,2)
b <- c(2, 4)
x <- solve(A, b) # c(-1 3)

A.inverse <- solve(A)
A.inverse %*% A # = I
qr(A) # QR decomposition
chol(A) # Cholesky decomposition
det(A) # Determinant
eigen(A) # Eigenvals and vectors
#sweep(A, ...) # Numerical analysis sweep operations
E <- matrix(1:9, 3, 3)
v <- c(1, 2, 3)
sweep(E, 1, v, '+') # Adds v to each row of E

# Markov Chain States
# Let p be the transition-state matrix [i, j] = [from , to]
findpi1 <- function(p) {
	n <- nrow(p)
	imp <- diag(n) - t(p)
	imp[n, ] <- rep(1, n)
	rhs <- c(rep(0, n - 1), 1)
	pivec <- solve(imp, rhs)
	pivec
}

# Example for 3-heads game 
M <- matrix(c(0.5, 0.5, 0.5, 0.5, 0, 0.5, 0, 0.5, 0), 3)
findpi1(M)

# Same solution using eigevals
findpi2 <- function(p) {
	n <- nrow(p)
	# Find 1st eigenvect of t(p)
	pivec <- abs(eigen(t(p))$vectors[,1])
	# Normalize too sum to 1
	pivec <- pivec / sum(pivec)
	pivec
}

findpi2(M)

# Set Operations
x <- c(1,3,5,7,9)
y <- c(1,2,3,5,7)
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y)
x %in% y

# combinations
(c1 <- combn(1:3, 1))
(c2 <- combn(1:3, 2))
(c3 <- combn(1:3, 3))

# Simulation
nreps <- 100000
sum <- numeric(nreps + 1)
div <- numeric(nreps + 1)
sum[1] <- div[1] <- 0
for (i in 2:(nreps + 1)) {
	xy <- rnorm(2)
	sum[i] <- sum[i - 1] + max(xy)
	div[i] <- sum[i] / (i - 1)
}

par(mfrow=c(2,1))
plot(sum, type='l')
plot(div, type='l', col=2)


# 9: Object Oriented Programming
# Example: OOP in the lm() Function
x <- 1:3
y <- c(1, 3, 8)
lmout <- lm(y ~ x)
lmout
class(lmout)
unclass(lmout)

# Finding the Implementations of Generic Methods
methods(print)
wrds <- 'which word is mispelled?'
aspell(wrds)

# Writing S3 Classes
j <- list(name='Joe', salary=55000, union=T)
class(j) <- 'employee'
attributes(j)
j
print.employee <- function(worker) {
	cat(worker$name,'\n')
	cat('salary', worker$salary, '\n')
	cat('union member', worker$union, '\n')
}
methods(, 'employee')
j

k <- list(name='Kate', salary=68000, union=F, hrsThisMonth=2)
class(k) <- c('hrlyEmployee', 'employee')
k

# Example: Upper-Triangular Matrix Class, ut
# Utility function: returns 1 + ... + i
sum1toi <- function(i) { return(i * (i + 1) / 2) }
# Create an object of class 'ut' from the full matrix inmat (0s and all)
ut <- function(inmat) {
	n <- nrow(inmat)
	rtrn <- list()
	class(rtrn) <- 'ut'
	rtrn$mat <- vector(length=sum1toi(n))
	rtrn$ix <- sum1toi(0:(n - 1)) + 1

	for (i in 1:n) {
		# store col i
		ixi <- rtrn$ix[i]
		rtrn$mat[ixi:(ixi + i -1)] <- inmat[1:i, i]
	}
	
	return(rtrn)
}

# Uncompress utmat to full matrix
expandut <- function(utmat) {
	n <- length(utmat$ix)
	fullmat <- matrix(nrow=n, ncol=n)
	for (j in 1:n) {
		# Fill jth col
		start <- utmat$ix[j]
		fin <- start + j - 1
 		abovediagj <- utmat$mat[start:fin]
 		fullmat[, j] <- c(abovediagj, rep(0, n - j))
	}
	
	return(fullmat)
}

# print matrix
print.ut <- function(utmat) { print(expandut(utmat)) }

# mult one ut mat bay another, returning another ut instance
# implement as binary operation
'%mut%' <- function(utmat1, utmat2) {
	n <- length(utmat1$ix)
	utprod <- ut(matrix(0, n, n))
	for (i in 1:n) {
		# let a[j] and bj denote cols j of utmat1 and 2 resp.
		# e.g. b2[1] means elem 1 of col 2 of ut 2
		# then col i of prod is equal to:
		#bi[1]a[1] + ... + bi[i]a[i]
		# find index of start of col i in utmat2
		startbi <- utmat2$ix[i]
		# init vect tat will become bi[1]a[1] + ... + bi[i]a[i]
		prodcoli <- rep(0, i)

		for (j in 1:i) {
			# find bi[j]a[j], add to prodcoli
			startaj <- utmat1$ix[j]
			bielement <- utmat2$mat[startbi + j - 1]
			prodcoli[1:j] <- prodcoli[1:j] + bielement * utmat1$mat[startaj:(startaj + j - 1)]
		}
		
		# now tack on 0s
		startprodcoli <- sum1toi(i - 1) + 1
		utprod$mat[startbi:(startbi + i - 1)] <- prodcoli
	}
	
	return(utprod)
}

test <- function() {
	utm1 <- ut(rbind(1:2, c(0,2)))
	utm2 <- ut(rbind(3:2, c(0,1)))
	utp <- utm1 %mut% utm2
	print(utm1)
	print(utm2)
	print(utp)
	utm1 <- ut(rbind(1:3, 0:2, c(0,0,5)))
	utm2 <- ut(rbind(4:2, 0:2, c(0,0,1)))
	utp <- utm1 %mut% utm2
	print(utm1)
	print(utm2)
	print(utp)
}

test()


# Ch. 10 I/O
scan('~/Desktop/R/z1.txt')
scan('~/Desktop/R/z2.txt')
scan('~/Desktop/R/z3.txt', what='')	# expect string data
scan('~/Desktop/R/z4.txt', what='')	# reads all data as strings
v <- scan('~/Desktop/R/z1.txt')
# whitespace is default separator; to use other use sep= attribute
x2 <- scan('~/Desktop/R/z3.txt', what='', sep='\n')
# to read from keyboard
keyIn <- scan()
# Read a line of text from keyboard
k2 <- readline()
# As prompt 
inits <- readline("Input your intials: ")
print('abc')
cat('abc\n')	# output not numbered
x <- 1:3
cat(x, 'abc', 'd\nef')
cat(x, 'abc', 'd\nef', sep='')
cat(x, 'abc', 'd\nef', sep='\n')

# Reading a Data Frame or Matrix fr a file
z <- read.table('~/Desktop/R/z.txt', header=T)
z
x <- matrix(scan('~/Desktop/R/x.txt'), nrow=8, byrow=T) # or:
x <- as.matrix(read.table('~/Desktop/R/x.txt'))
z1 <- readLines('~/Desktop/R/z.txt')
z1	# a vector of strings, each corresponding to a row in z.txt
# To read line by line, must establish a connection

# Introduction to Connections
# created by calling file(), url(), etc.; see ?connection for details
c <- file('~/Desktop/R/z.txt', 'r')
readLines(c, n=1)
readLines(c, n=1)
readLines(c, n=1)
readLines(c, n=1)
readLines(c, n=1)

# or...
c <- file('~/Desktop/R/z.txt', 'r')
while(T) {
	rl <- readLines(c, n=1)
	if (length(rl) == 0) {
		print ("reached the end")
		break
	} else { print(rl) }
}

# To 'rewind' and start at beginning of file, use seek():
c <- file('~/Desktop/R/z.txt', 'r')
readLines(c, n=2)
seek(con=c, where=0)	# returns location of pointer prior to the call
readLines(c, n=1)
close(c) # close the connection

# Extended Example: Reading PUMS Census Files
# read in PUMS file pf, extracting Person records, returning data.frame; ea row of output will
# consisit of the household Ser. no. and the fields specified in the list flds; the cols of the
# data.frame will have the names of the indices in flds
extractPums <- function(pf, flds) {
	df <- data.frame()
	con <- file(pf, 'r') # make connection
	
	# process input file
	repeat {
		hrec <- readLines(con, 1) # read household record
		if (length(hrec) == 0) { break } # EOF
		
		# get household ser. no.
		serNo <- intExtract(hrec, c(2, 8))
		# how many Person records?
		npr <- intExtract(hrec, c(106, 107))
		if (npr > 0) {
			for (i in 1:npr) {
				prec <- readLines(con, 1) # get Person record
				# make this person's row for the df
				person <- makeRow(serNo, prec, flds)
				# add to df
				df <- rbind(df, person)
			}
		}
		
		return(dtf)
	}
	
	# set up this persons row for the df
	makerow <- function(srn, pr, fl) {
		l <- list()
		l[['serNo']] <- srn
		for (nm in names(fl)) {
			l[[nm]] <- intExtract(pr, fl[[nm]])
		}
		
		return(l)
	}
	
	# Extract an int field in the String s, in char positions rng[1:2]
	intExtract <- function(s, rng) {
		fld <- substr(s, rng[1], rng[2])
		return(as.integer(fld))
	}
}
pumsdf <- extractPums('pumsa', list(Gender=c(23, 23), Age=c(25,26)))



# Graphics
hist(rnorm(60))
pt <- locator(1)
pt

# polygons
f <- function(x) { return(1 - exp(-x)) }
curve(f, 0, 2)
polygon(c(1.2, 1.4, 1.4, 1.2), c(0, 0, f(1.3), f(1.3)), col='cyan', border='blue')
polygon(c(1.0, 1.2, 1.2, 1.0), c(0, 0, f(1.1), f(1.1)), col='blue', density=10, 
		border='cyan')
x <- sort(runif(100000, 0, 2))
y <- f(x) + rnorm(100000, sd=0.3)
plot(y ~ x, cex=0.6, col=rgb(0,0,0,0.07), pch=16)
curve(f, 0, 2, add=T, col=2)
lines(lowess(x, y), col=4)

# Graphing Explicit Functions
g <- function(t) { return((t^2 + 1)^0.5) }
x <- seq(0, 5, length=10000)
y <- g(x)
plot(y ~ x, type='l')

# alternately:
curve((x^2 + 1)^0.5, 0, 5)
# or:
plot(g, 0, 5)


# 3D Plots
a <- 1:10
b <- 1:15
eg <- expand.grid(x=a, y=b)
eg$z <- eg$x^2 + eg$x * eg$y
wireframe(z ~ x + y, eg)	# library: lattice
wireframe(z ~ x + y, eg, shade=T)