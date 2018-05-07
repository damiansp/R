rm(list=ls())
setwd('~/Learning/R/RLearning/ggplot2/')

library(ggplot2)


# 2 Fuel Economy Data
data(mpg)



# 3 Building a Scatterplot
qplot(displ, hwy, data=mpg, color=factor(cyl))



# 4 A More Complex Plot
qplot(displ, hwy, data=mpg, facets=. ~ year) + geom_smooth()



# 6 Data Structures
p <- qplot(displ, hwy, data=mpg, color=factor(cyl))
summary(p)
save(p, file='data/plot.rdata')
load('data/plot.rdata')
ggsave('data/plot.png', width=5, height=5)
