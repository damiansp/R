rm(list=ls())

library(ggplot2)


# 2 Fuel Economy Data
data(mpg)



# 3 Building a Scatterplot
qplot(displ, hwy, data=mpg, color=factor(cyl))



# 4 A More Complex Plot
qplot(displ, hwy, data=mpg, facets=. ~ year) + geom_smooth()



# 5 Components of a Layered Grammar
