#===========================================================#
#															#
#	Program to Accompany "Linear Algebra with Application"	#
#															#
#===========================================================#

rm(list = ls())

#install.packages('editrules', dependencies = T, 
#				 repos = 'http://cran.us.r-project.org')
library(editrules)
# library(Matrix)
load('~/Desktop/R/LinearAlgebra/LinearAlgebra.RData')

#===================================#
#									#
#	Chapter 1:	Linear Systems		#
#									#
#===================================#

# 1.1 Introduction to Linear Systems
### p 12 ###
# Solve the system:
	# 3x[1] + 2x[2] = 1
	# 2x[1] +  x[2] = 3
(M = matrix(c(3, 2, 2, 1), nrow = 2)) # Matrix of X
solve(M, c(1, 3)) # c(1, 3) = y
# >> x[1] = 5, x[2] = -7


# 1.2 Gauss Elimination
### p 26 ###
# Reduce matrix to its reduced row echelon equivalent
(M = matrix(c(1, 2, 3, 2, 2, 3, 3, 3, 3), nrow = 3))
echelon(M)


# 1.3 Numerical Solutions
### p 36 ###
(A = matrix(c(3, 2, 2, -1), nrow = 2))
(B = matrix(c(1, -3), nrow = 2))
solve(A, B)



#===================================#
#									#
#	Chapter 2: Vectors				#
#									#
#===================================#

# 2.1 Vector Operations
# Addition/Subtraction of vectors and mulitplying by scalars
u <- c(1, 2, 3)
v <- c(-3, 2, -1)
2 * u - 5 * v

# 2.2 The Dot Product
# For vectors this is sum(v1 * v2) = t(v1) %*% v2 = v1 %*% v2
dot.prod <- function(u, v) {
	sum(u * v)
}

# All of the following are the same
dot.prod(u, v)
u %*% v
t(u) %*% v


# Exercises 2.2
u <- c(-1, 2, -2)
v <- c(4, -3, 5)
w <- c(-4, -2, 0)
d <- c(-1, -2, 1, sqrt(3))

v.norm <- function(v) {
	sqrt(sum(v^2))
}

# 1.
v.norm(u)	# 3
v.norm(v)	# 7.02
v.norm(w)	# 4.47
v.norm(u + v)	# 4.36
v.norm(u - v)	# 9.95
v.norm(u - v + w)	# 11.79
v.norm(d)	# 3
v.norm(10 * d)	# 30
v.norm(d) * d	# [-2, -6, -3, 3rt(3)]

# 3. 
u %*% v	# -20
w %*% u	# 0
u %*% (v + w) # -20
(u %*% v) * (v %*% w)	# 200
d %*% d	# 9
d %*% d * d	# [-9, -18, 9, 15.59]

# return a unit vector in the same direction as v
unit.directional <- function(v) {
	return ((1 / v.norm(v)) * v)
}

# 5.
unit.directional(u)	# [-1/3, 2/3, -2/3]
unit.directional(v)	# [0.57, -0.42, 0.71]
unit.directional(w)	# [-0.89, -0.45, 0]
unit.directional(d) # [-1/3, -2/3, 1/3, 0.58]

# 9. Find vector of length 9 in direction of d
9 * unit.directional(d)	# [-3, -6, 3, 5.20]



save.image('~/Desktop/R/LinearAlgebra/LinearAlgebra.RData')