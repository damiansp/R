#===================================#
#									#
#	Modern Optimization with R		#
#		Paulo Cortez	2014		#
#									#
#===================================#

#=======================#
#						#
#	3. Blind Search		#
#						#
#=======================#

rm(list = ls())
load('~/Desktop/R/Optimization/optimization.RData')


# 3.2 Full Blind Search

# Full blind search method
#	search: matrix with solutions x D
#	FUN:	evaluation function
#	type:	'min' or 'max'
#	...:	extra FUN params
fsearch <- function(search, FUN, type = 'min', ...) {
  # Run FUN over all search rows
  x <- apply(search, 1, FUN, ...)
  ib <- switch(type, min = which.min(x), max = which.max(x))
  return(list(index = ib, sol = search[ib, ], eval = x[ib]))
}


# Depth-first full search method
#	l:		level of tree
#	b:		branch of tree
#	domain:	vector list of size D with domain vals
#	FUN:	eval function
#	type:	'min' or 'max'
#	D:		dimension (no. of vars)
#	x:		current solution vector
#	bcur:	current best solution
#	...:	extra FUN params
dfsearch <- function(
  l = 1, b = 1, domain, FUN, type = 'min', D = length(domain), x = rep(NA, D),
  bcur = switch(type, 
  				min = list(sol = NULL, eval = Inf), 
  				max = list(sol = NULL, eval = -Inf)),
  ... 
) {
  if ((l - 1) == D) {
  	f <- FUN(x, ...)
  	fb <- bcur$eval
  	ib <- switch(type, min  = which.min(c(fb, f)), max = which.max(c(fb, f)))
  	
  	if (ib == 1) {
  	  return(bcur)
  	} else {
  	  return(list(sol = x, eval = f))
  	}
  } else {
  	for (j in 1:length(domain[[l]])) {
  	  x[l] <- domain[[l]][j]
  	  bcur <- dfsearch(l + 1, j, domain, FUN, type, D = D, x = x, bcur = bcur, ...)
  	}
  	
  	return(bcur)
  }
}

# Read D bits from int x
binint <- function(x, D) {
  # Get D bits
  x <- rev(intToBits(x)[1:D])
  # remove extra 0s from raw type
  as.numeric(unlist(strsplit(as.character(x), ''))[(1:D) * 2])
}

# Convert binary vector into integer
intbin <- function(x) {
  sum(2^(which(rev(x == 1)) - 1))
}

# Sum a raw binary obj x (evaluation function)
sumbin = function(x) {
  sum(as.numeric(x))
}

# Max sin of raw binary obj x (eval func)
maxsin = function(x, Dim) {
  sin(pi * (intbin(x)) / (2^Dim))
}


# set dims
D <- 8
# int search space
x <- 0:(2^D - 1)

# Set full search space in solutions x D
search <- t(sapply(x, binint, D = D))

# Set domain vals (D binary vars)
domain <- vector('list', D)

for (i in 1:D) {
  domain[[i]] <- c(0, 1)
}

# Sum of bits -- fsearch
S1 <- fsearch(search, sumbin, 'max')
cat('fsearch best s:', S1$sol, 'f:', S1$eval, '\n')

# Sum of bits -- dfsearch
S2 <- dfsearch(domain = domain, FUN = sumbin, type = 'max')
cat('fsearch best s:', S2$sol, 'f:', S2$eval, '\n')

# max sin -- fsearch
S3 <- fsearch(search, maxsin, 'max', Dim = 8)
cat('fsearch best s:', S3$sol, 'f:', S3$eval, '\n')

# max sin -- dfsearch
S4 <- dfsearch(domain = domain, FUN = maxsin, type = 'max', Dim = 8)
cat('fsearch best s:', S4$sol, 'f:', S4$eval, '\n')

# Bag problem functions
# Compute cost for producing units
#	units:	no. of units produced
#	A:		fixed costs
#	cpu:	cost per unit
cost <- function(units, A = 100, cpu = 35 - 5 * (1:length(units))) {
  return(A + cpu * units)
}

# Compute te estimated sales for x
#	x:	vector of prices
#	m:	marketing effort
#	A, B, C:	constants of the estimated function
sales <- function(x, A = 1000, B = 200, C = 141, 
				  m = seq(2, length = length(x), by = -0.25)) {
  return(round(m * (A / log(x + B) - C)))				  	
}

# Compute the bag factory profit for x:
#	x:	a vector of prices
profit <- function(x) {
  x <- round(x)
  s <- sales(x)
  c <- cost(s)
  profit <- sum(s * x - c)
  return(profit)
}

# Set optimum price for one bag type (D), assuming an independent influence of a
# particular price on the the remaining bag prices
ibag <- function(D) {
  # Price for each bag type
  x <- 1:1000
  
  # Set search space for one bag
  search <- matrix(data = 1, ncol = 5, nrow = 1000)
  search[, D] <- x
  S1 <- fsearch(search, profit, 'max')
  # Return best price
  return(S1$sol[D])
}

# Compute best price for all bag types
S <- sapply(1:5, ibag)

# Show optimum solution
cat('optimum s:', S, 'f:', profit(S), '\n')



# 3.3 Grid Search

# Standard grid search method (uses fsearch)
# 	step:	vector with step size for each dim D
#	lower:	vector with lowest val for each dim
#	upper:	........... highest ...............
#	FUN:	evaluation function
#	type: 	'min' or 'max'
#	...:	extra FUN params
gsearch <- function(step, lower, upper, FUN, type = 'min', ...) {
  D <- length(step)
  domain <- vector('list', D)
  L <- vector(length = D)	
  
  for (i in 1:D) {
  	domain[[i]] <- seq(lower[i], upper[i], by = step[i])
  	L[i] <- length(domain[[i]])
  }  
  
  LS <- prod(L)
  s <- matrix(ncol = D, nrow = LS)
  
  for (i in 1:D) {
  	if (i == 1) {
  		E <- 1
  	} else {
  		E <- E * L[i - 1]
  	}
  	
  	s[, i] <- rep(domain[[i]], length = LS, each = E)
  }
  
  fsearch(s, FUN, type, ...)
}

# Standard grid search method (uses dfsearch)
gsearch2 <- function(step, lower, upper, FUN, type = 'min', ...) {
  D <- length(step)
  domain <- vector('list', D)
  for (i in 1:D) {
  	domain[[i]] <- seq(lower[i], upper[i], by = step[i])
  }
  dfsearch(domain = domain, FUN = FUN, type = type, ...)
}

# Nested grid search method (uses fsearch)
#	levels: number of nested levels
ngsearch <- function(levels, step, lower, upper, FUN, type, ...) {
  stop <- F
  i <- 1
  # Best current:
  bcur <- switch(type, min = list(sol = NULL, eval = Inf), 
  					   max = list(sol = NULL, eval = -Inf))
  					   
  while (!stop) {
    s <- gsearch(step, lower, upper, FUN, type, ...)
    
    # update best current solution if needed:
    if ((type == 'min' && s$eval < bcur$eval) ||
    	(type == 'max' && s$eval > bcur$eval)) {
      bcur <- s		
    }
    
    # update step, lower, upper
    if (i < levels) {
      step <- step / 2
      interval <- (upper - lower) / 4
      lower <- sapply(lower, max, s$sol - interval)
      upper <- sapply(upper, min, s$sol + interval)
    }
    
    if (i >= levels || sum((upper - lower) <= step) > 0) {
      stop <- T
    } else {
    	i <- i + 1
    }
  }
  
  return (bcur)
}

# Grid search for all bag prices with 100$ steps
PTM <- proc.time()
S1 <- gsearch(rep(100, 5), rep(1, 5), rep(1000, 5), profit, 'max')
sec <- proc.time() - PTM
cat('gsearch best s:', S1$sol, 'f:', S1$eval, 'time:', sec[3], 's\n')

# Same using gsearch2
PTM <- proc.time()
S1 <- gsearch2(rep(100, 5), rep(1, 5), rep(1000, 5), profit, 'max')
sec <- proc.time() - PTM
cat('gsearch2 best s:', S1$sol, 'f:', S1$eval, 'time:', sec[3], 's\n')

# ...and using ngsearch
PTM <- proc.time()
S1 <- ngsearch(3, rep(500, 5), rep(1, 5), rep(1000, 5), profit, 'max')
sec <- proc.time() - PTM
cat('ngsearch best s:', S1$sol, 'f:', S1$eval, 'time:', sec[3], 's\n')






save.image('~/Desktop/R/Optimization/optimization.RData')