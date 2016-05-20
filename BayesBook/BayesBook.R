rm(list=ls())



# 2.1
nbr <- read.csv('~/Desktop/R/BayesBook/data/sscsample.data.csv')
nbr$stratum <- as.factor(nbr$stratum)
nbr$cluster <- as.factor(nbr$cluster)

names(nbr)	# value = income; stratum = race; cluster = neighborhood
popMeanIncome <- mean(nbr$value)	# 8.994
hist(nbr$value)

# (a)
boxplot(nbr$value ~ nbr$stratum)
tapply(nbr$value, nbr$stratum, mean)	# 1: 10.31 (40%)		2: 8.41 (40%)		3: 7.53 (20%)

# (b)
nSamp <- 200
n <- 20
records <- dim(nbr)[1]

indices <- sample(records, n, replace=T)
samp <- nbr[indices, ]

for (i in 2:nSamp) {
	indices <- sample(records, n, replace=T)
	nextSamp <- nbr[indices, ]
	samp <- cbind(samp, nextSamp)
}

	# (b i)
	strataCols <- seq(2, 599, by=3)
	frac1 <- frac2 <- frac3 <- c()
	for (cl in strataCols) {
		tbl <- table(samp[, cl])
		frac1 <- c(frac1, tbl[1] / n)
		frac2 <- c(frac2, tbl[2] / n)
		frac3 <- c(frac3, tbl[3] / n)
	}
	
	# (b ii)
	mean(frac1); sd(frac1); hist(frac1)	# mean = 0.39; sd = 0.11
	mean(frac2); sd(frac2); hist(frac2)	# mean = 0.39; sd = 0.12
	mean(frac3); sd(frac3); hist(frac3)	# mean = 0.39; sd = 0.08
	
	# (b iii)
	

