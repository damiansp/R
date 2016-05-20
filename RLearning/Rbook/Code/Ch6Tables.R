#################
#               #
#  Ch 6 Tables  #
#               #
#################

load("RBook.RData")

#Summary tables
data <- read.table('~/Desktop/Hackery/R Files/Rbook/Files/daphnia.txt', header=T)
attach(data)
names(data)
tapply(Growth.rate, Detergent, mean)
tapply(Growth.rate, Water, mean)
tapply(Growth.rate, Daphnia, mean)
tapply(Growth.rate, list(Daphnia, Detergent), mean)
tapply(Growth.rate, list(Daphnia, Detergent), function(x) sqrt(var(x)/length(x)))	#function = SE
tapply(Growth.rate, list(Daphnia, Detergent, Water), mean)
ftable(tapply(Growth.rate, list(Daphnia, Detergent, Water), mean))	#flat table
tapply(Growth.rate, Detergent, mean, trim=0.1)
dets <- as.vector(tapply(as.numeric(Detergent), list(Detergent, Daphnia), mean))
levels(Detergent)[dets]
clones <- as.vector(tapply(as.numeric(Daphnia), list(Detergent, Daphnia), mean))
levels(Daphnia)[clones]
tapply(Growth.rate, list(Detergent, Daphnia), mean)	#convert to single 'means' vector:
means <- as.vector(tapply(Growth.rate, list(Detergent, Daphnia), mean))
detergent <- levels(Detergent)[dets]
daphnia <- levels(Daphnia)[clones]
data.frame(means, detergent, daphnia)
#Also achievable by:
new <- as.data.frame.table(tapply(Growth.rate, list(Detergent, Daphnia), mean))
names(new) <- c('detergents', 'daphnia', 'means')

#Tables of counts
cells <- rnbinom(10000, size=0.63, prob=0.63/1.83) #size = aggregation param, k; prob = k/(mean + k); mean = 0.63
hist(cells)
table(cells)
gender <- rep(c('male', 'female'), c(5000, 5000))
table(cells, gender)
tapply(cells, gender, mean)

#Expanding a table into a dataframe
count.table <- read.table('~/Desktop/Hackery/R Files/Rbook/Files/tabledata.txt', header=T)
attach(count.table)
names(count.table)
count.table
dbtable <- as.data.frame(lapply(count.table, function(x) rep(x, count.table$count)))
dbtable <- dbtable[,-1]

#Converting from a dataframe to a table
table(dbtable)
as.data.frame(table(dbtable))


save.image("RBook.RData")
