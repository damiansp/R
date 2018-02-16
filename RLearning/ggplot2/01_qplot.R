rm(list=ls())
library(ggplot2)

d.small <- diamonds[sample(nrow(diamonds), 500), ]
qplot(carat, price, data=diamonds)
qplot(carat, price, data=diamonds, alpha=I(1 / 100))
qplot(log(carat), log(price), data=diamonds, alpha=I(1 / 100))
qplot(carat, x*y*z, data=diamonds)

qplot(log(carat), log(price), data=d.small, color=color)
qplot(log(carat), log(price), data=d.small, color=cut, shape=cut)