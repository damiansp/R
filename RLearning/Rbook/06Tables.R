#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Rbook')



#Summary tables
data <- read.table('data/daphnia.txt', header=T)
names(data)
tapply(data$Growth.rate, data$Detergent, mean)
tapply(data$Growth.rate, data$Water, mean)
tapply(data$Growth.rate, data$Daphnia, mean)
tapply(data$Growth.rate, list(data$Daphnia, data$Detergent), mean)
tapply(data$Growth.rate, 
       list(data$Daphnia, data$Detergent), 
       function(x) sqrt(var(x)/length(x)))	#function = SE
tapply(data$Growth.rate, list(data$Daphnia, data$Detergent, data$Water), mean)
ftable(tapply(
  data$Growth.rate, list(data$Daphnia, data$Detergent, data$Water), mean))
water <- factor(data$Water, levels=c('Wear', 'Tyne'))
ftable(tapply(data$Growth.rate, list(data$Daphnia, data$Detergent, water), mean))
tapply(data$Growth.rate, data$Detergent, mean, trim=0.1)
dets <- as.vector(tapply(
  as.numeric(data$Detergent), list(data$Detergent, data$Daphnia), mean))
levels(data$Detergent)[dets]
clones <- as.vector(tapply(
  as.numeric(data$Daphnia), list(data$Detergent, data$Daphnia), mean))
levels(data$Daphnia)[clones]
# convert to single 'means' vector:
tapply(data$Growth.rate, list(data$Detergent, data$Daphnia), mean)	
means <- as.vector(
  tapply(data$Growth.rate, list(data$Detergent, data$Daphnia), mean))
detergent <- levels(data$Detergent)[dets]
daphnia <- levels(data$Daphnia)[clones]
data.frame(means, detergent, daphnia)

# Also achievable by:
new <- as.data.frame.table(
  tapply(data$Growth.rate, list(data$Detergent, data$Daphnia), mean))
names(new) <- c('detergents', 'daphnia', 'means')

# Tables of counts
# size = aggregation param, k; prob = k/(mean + k); mean = 0.63
cells <- rnbinom(10000, size=0.63, prob=0.63/1.83) 
hist(cells)
table(cells)
gender <- rep(c('male', 'female'), c(5000, 5000))
table(cells, gender)
tapply(cells, gender, mean)

# Expanding a table into a dataframe
count.table <- read.table('data/tabledata.txt', header=T)
names(count.table)
count.table
lapply(count.table, function(x) rep(x, count.table$count))
dbtable <- as.data.frame(lapply(count.table, 
                                function(x) rep(x, count.table$count)))
dbtable <- dbtable[,-1]



# 4. Converting from a dataframe to a table
table(dbtable)
frame <- as.data.frame(table(dbtable))
names(frame)[4] <- 'count'

counts <- matrix(c(2, 2, 3, 4, 1, 4, 2, 0, 1, 5, 3, 3), nrow=4)
counts
prop.table(counts, 1) # rowwise
prop.table(counts, 2) # colwise
prop.table(counts)    # tablewise



# 6. Scale
scale(counts)
apply(counts, 2, sd) # sd in ea col



# 7. expand.grid
expand.grid(
  height=seq(60, 80, 5), weight=seq(100, 300, 50), sex=c('male', 'female'))



# 9. Comparing table and tabulate
x <- c(2, 2, 2, 7, 7, 11)
table(x)
tabulate(x)
# negative numbers will be ignored
x <- c(-1, -2, x, -5)
tabulate(x)

table(rnbinom(100, 1, 0.2))
tabulate(rnbinom(100, 1, 0.2) + 1, 30)

totals <- numeric(1000)
for (i in 1:1000) totals[i] <- sum(tabulate(rnbinom(100, 1, 0.2) + 1, 30))
table(totals)