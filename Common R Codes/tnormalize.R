# Various methods for transforming t distribution with nu degrees of freedom to standard normals

# Quenouille:
tnorm1 <- function(tdist, nu) {
	# find the sign of the original values:
	orig.sign <- tdist >= 0 # 1 = +, 0 = -
	
	# tranform	
	val <- sqrt(nu - 1) * asinh(sqrt(tdist^2 / nu))
	
	# find sign of transformed values
	val.sign <- val >= 0
	
	# make sure signs of transformed values are the same as the original, if not multiply by -1
	swap.sign <- which(orig.sign != val.sign)
	if (length(swap.sign > 0)) {
		val[swap.sign] <- val[swap.sign] * -1
	}
	
	# returns the transformed values and the Shapiro-Wilk test for normality for both the original
	# and transformed data
	return ( list(transform=val, shapiro.original=shapiro.test(tdist), 
				  shapiro.transform=shapiro.test(val)) )
}



# Anscombe
tnorm2 <- function(tdist, nu) {
	orig.sign <- tdist >= 0 # 1 = +, 0 = -
	val <- sqrt((2*nu - 1) / 3) * asinh(sqrt((3*tdist^2) / 2*nu))
	val.sign <- val >= 0
	# give approrpriate sign 
	swap.sign <- which(orig.sign != val.sign)
	if (length(swap.sign > 0)) {
		val[swap.sign] <- val[swap.sign] * -1
	}
	
	return ( list(transform=val, shapiro.original=shapiro.test(tdist), 
				  shapiro.transform=shapiro.test(val)) )
}



# Chu:
tnorm3 <- function(tdist, nu) {
	orig.sign <- tdist >= 0 # 1 = +, 0 = -
	val <- sqrt(nu * log(1 + tdist^2/nu))
	val.sign <- val >= 0
	# give approrpriate sign 
	swap.sign <- which(orig.sign != val.sign)
	if (length(swap.sign > 0)) {
		val[swap.sign] <- val[swap.sign] * -1
	}
	
	return ( list(transform=val, shapiro.original=shapiro.test(tdist), 
				  shapiro.transform=shapiro.test(val)) )
}



# Wallace
tnorm4 <- function(tdist, nu) {
	orig.sign <- tdist >= 0 # 1 = +, 0 = -
	val <- ((8*nu + 1) / (8*nu + 3)) * sqrt(nu * log(1 + tdist^2/nu))
	val.sign <- val >= 0
	# give approrpriate sign 
	swap.sign <- which(orig.sign != val.sign)
	if (length(swap.sign > 0)) {
		val[swap.sign] <- val[swap.sign] * -1
	}
	
	return ( list(transform=val, shapiro.original=shapiro.test(tdist), 
				  shapiro.transform=shapiro.test(val)) )
}

tnorm4Inverse <- function(t.trans, nu) {
	orig.sign.neg <- which(t.trans < 0) # 1 = +, 0 = -
	out <- sqrt(nu * ((exp(((t.trans/((8*nu + 1) / (8*nu + 3)))^2) / nu) - 1)))
	out[orig.sign.neg] <- out[orig.sign.neg] * -1
	out
}



# Test:
# create a random t distribution--size = 100, 3 d.f.:
#set.seed(4)
#n <- 100
#nu <- 3
#s <- rt(n, nu)			# p = 0.0000606
#s1 <- tnorm1(s, nu)		# p = 0.101
#s2 <- tnorm2(s, nu)		# p = 0.172
#s3 <- tnorm3(s, nu)		# p = 0.386
#s4 <- tnorm4(s, nu)		# p = 0.386

#par(mfrow=c(2,3))
#hist(s)
#hist(s1$transform)
#hist(s2$transform)
#hist(s3$transform)
#hist(s4$transform)

# In this case s3 and s4 are both about the same, and do a better job of normalizing than s1 or 
# s2
