obs2exp <- function(M) {
  # Takes a matrix (contincgency table) of observed values, and returns a matrix of 
  # expected frequencies
  M.exp <- M - M # initialize expected matrix to all 0s
  for (i in 1:(dim(M)[1])) {
    for (j in 1:(dim(M)[2])) {
      M.exp[i, j] <- (M[i,j]/sum(M[i,])) * (M[i,j]/sum(M[,j]))
    }
  }
  M.exp
}