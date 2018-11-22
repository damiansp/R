#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Rbook')

worms <- read.table('data/worms.txt', header=T)
worms <- worms[complete.cases(worms), ]
head(worms)
#by(worms[, c(2,3,5,7)], worms$Vegetation, mean) # ?
by(worms, worms$Vegetation, function(x)  lm(Worm.density ~ Soil.pH, data=x))

worms[order(worms$Slope), ]
worms[order(worms$Slope, worms$Vegetation), ]
worms[order(worms$Slope, -worms$Worm.density), ]
worms[, sapply(worms, is.factor)]

duplicated(worms$Vegetation)
worms$Vegetation[duplicated(worms$Vegetation)]


worms <- read.table('data/worms.txt', header=T, row.names=1)
head(worms)


date.str <- '1976-11-03'
date <- strptime(date.str, format='%Y-%m-%d')
date
class(date)

herbicides <- read.table('data/herbicides.txt', header=T)
head(herbicides)

herbicides$Herbicide[match(worms$Vegetation, herbicides$Type)]
worms$hb <- herbicides$Herbicide[match(worms$Vegetation, herbicides$Type)]
head(worms)

(lifeforms <- read.table('data/lifeforms.txt', header=T))
(flowering <- read.table('data/fltimes.txt', header=T))
merge(flowering, lifeforms) # inner join
(both <- merge(flowering, lifeforms, all=T)) # outer

(seeds <- read.table('data/seedwts.txt', header=T))
merge(both, seeds, by.x=c('Genus', 'species'), by.y=c('name1', 'name2'))


(frame <- read.table('data/sales.txt', header=T))
(people <- rowMeans(frame[, 2:5]))
(people <- people - mean(people))
(new.frame <- cbind(frame, people))


head(worms)
aggregate(worms[, c(1, 2, 4, 6)], by=list(veg=worms$Vegetation), mean)
