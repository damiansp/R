#========================================================================================#
#																						 #
#  Code developed from the text:														 #
#  Nello Cristianini and John Shawe-Taylor (2000)										 #
#  'An Introduction to Support Vector Machines and Other Kernel-Based Learning Methods'  #
#  Chapter 2: Linear Learning Machines													 #
#  @author Damian Satterthwaite-Phillips <damiansp@gmail.com>							 #
#  @version 13Apr2014																	 #
#																						 #
#========================================================================================#

# 2.1 Linear Classification
dotprod <- function(v1, v2) {
	return(sum(v1 * v2))
}

sgn <- function(f) {
	if (f >= 0) {
		return (1)
	} 
	
	return (-1)
}


	# 2.1.1 Rosenblatt's Perceptron
	vLength <- function(v) {
		return (sqrt(sum(v^2)))
	}
