rm(list=ls())

library(ggplot2)


# 2 Fuel Economy Data
data(mpg)



# 3 Building a Scatterplot
qplot(displ, hwy, data=mpg, color=factor(cyl))

# Scaling