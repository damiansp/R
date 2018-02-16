
# examples of basic procedures and objects in R@@@@@@@@@@@@@@@@@@@@@@

# R as calculator (most basic kind of evaluation)
5+2

# using a *function()*
sqrt(463)

# using a *constant*
2*pi

#evaluation that results in a graphics call...
plot(pi, 2*pi)

# *variables* acquire value through *assignment*
v <- 2*pi
v
v <- 2*pi; v
(v <- 2*pi)

# be careful with protected names (see Dalgaard p.4)
c <- 145

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#important objects (including vectors, arrays, matrices, dataframes)

#VECTOR
  #containing 'logical' data
  vLog <- c(T, F, T, T, F)
  vLog
  
  #numeric data
  vNum <- seq(1:10); vNum
  
  #character data
  vChar <- rep("M", 10); vChar
  vChar2 <- c(rep("M", 10), rep("F", 8)); vChar2

  #how many elements are in the vector?
  length(vNum)

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#MATRIX
  ?matrix #how do we make one?
  v1 <- seq(1:25)
  m1 <- matrix(v1, nrow=5); m1  
  m1 <- matrix(v1, nrow=5, byrow=T); m1  

  (m2 <- matrix(rep("T", 25), nrow=5))  #is this still a matrix?
  is.matrix(m2)
  
  #how many elements in it?
  length(m2)
  nrow(m2)
  ncol(m2)

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#INDEXING/SUBSETTING

  #make a vector
  a1 <- seq(1:24)
  a1
  a1[16]
  a1[2:5]
  a1[-1]

  #redimension it (2-d)
  dim(a1) <- c(12, 2); a1

  a1 #a1[r, c]
  a1[,1] #a1[, c=1]
  a1[1,] #a1[r=1, ]
  a1[,]
  a1[1:2,]

  #conditional selection
  sel <- a1[,1] >= 6; sel
  a1[sel,]
  a1[a1[,1] >= 6,]

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#DATAFRAME
  a <- runif(10); round(a, 2)
  b <- runif(10)
  c <- runif(10)
  
  #make a matrix first
  m3 <- cbind(a, b, c); m3
  is.matrix(m3) #yup
  plot(m3)

  #make a dataframe using the same data
  d1 <- data.frame(a, b, c); d1
  plot(d1)
  names(d1)
  
  #examine the structure
  str(d1)
  
  #naming convention for variables in dataframes
  d1$a
  
# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#FACTOR
  k <- runif(10, 0, 4)
  k
  round(k,2)
  k <- ceiling(k); k
  plot(k) #note effect of plotting integers
  
  #make the factor
  f <- factor(k, levels = 1:4); f
  plot(f) #note effect of plotting factor

  #add the factor to an existing dataframe
  d2 <- cbind(d1, f)
  d2
  str(d2)

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#VECTORIZATION (vector arithmetic, recycling)

  #make a vector object and inspect it
  s1 <- seq(1, 10); s1
  s2 <- runif(10); s2

  #divide one vector by another
  s1/s2

  #apply a constant to all members (recycling example)
  s1/10
  #more obvious recycling
  s1
  s1/c(1,10)

  #vectorization with matrices!!
  ml1 <- matrix(seq(1:25), nrow=5)
  j1 <- seq(1:5)
  ml1; j1
  ml1*j1 #j1 applied to each column in ml1
  t(t(ml1)*j1) #j1 applied to each row in ml1
  
  #how would we use this to "row standardize" a matrix?? (use rowSums())

#CREATING FUNCTIONS
fMean <- function(V) { #calculate the mean of a vector
  s <- sum(V)
  n <- length(V)
  return(s/n)
  }

mt1 <- runif(20)
fMean(mt1)
mean(mt1)
fMean(mt1) == mean(mt1)

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#FLOW CONTROL+++++++++++++++++++++++++++++++++++++++

#if() example
if(x<0) x <- x*-1 #convert to absolute value

#for() example (embedded in a function)

ourSum <- function(q) {
  tot <- 0
  for (i in 1:length(q)) tot <- tot + q[i]
  return(tot)
  }

# > BACK TO POWERPOINT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#HELP!!!!!!!!!!!
?xtabs #?function
?base #?library
help(plot) #help(topic)
help.search("plot") #help.search(“topic”)
help(abline, package=graphics) #help(function, package=PKG)
apropos("tab") #apropos(“string”)
find("table") #find(“function”)
find("xtabs")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#in-class exercise*******************************************
#read some data
dat1 <- read.table("F:/D830 backups Fall 2008/Robertson/courses/Data Analysis/Data Analysis 2009/lectures/W1L1 data.dat", sep=",", header=T)

#make a histogram
hist(dat1$income)
br1 <- seq(16000, 30000, by=1000); br1
hist(dat1$income, breaks=br1, col="grey")

#reformat the factor
str(dat1$rel)
levels(dat1$rel) <- c("Catholic", "Protestant")
str(dat1)

#examine data in a stripchart
stripchart(dat1$income~dat1$rel, method="jitter", jitter=.1, xlab="income", ylab="community")

#ditto, using a boxplot
boxplot(dat1$income~dat1$rel, notch=T)

#do a t-test!!
tt1 <- t.test(dat1$income~dat1$rel)
tt1
#********************************************************************