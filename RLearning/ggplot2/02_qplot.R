rm(list=ls())
library(ggplot2)
library(mgcv)

d.small <- diamonds[sample(nrow(diamonds), 500), ]


# 2.3 Basics 
qplot(carat, price, data=diamonds)
qplot(carat, price, data=diamonds, alpha=I(1 / 100))
qplot(log(carat), log(price), data=diamonds, alpha=I(1 / 100))
qplot(carat, x*y*z, data=diamonds)


# 2.4 Aesthetics: Color, Shape, Size
qplot(log(carat), log(price), data=d.small, color=color)
qplot(log(carat), log(price), data=d.small, color=cut, shape=cut)


# 2.5 Plot Geoms
# 2.5.1 Add a smoother
qplot(carat, price, data=d.small, geom=c('point', 'smooth'))
qplot(log(carat), log(price), data=d.small, geom=c('point', 'smooth'))
qplot(log(carat), 
      log(price), 
      data=diamonds, 
      geom=c('point', 'smooth'), 
      alpha=I(1/100))
qplot(log(carat), log(price), data=d.small, geom=c('point', 'smooth'), span=0.2)
qplot(log(carat), 
      log(price), 
      data=d.small, 
      geom=c('point', 'smooth'), 
      method='gam', 
      formula=y ~ s(x))
qplot(log(carat), 
      log(price), 
      data=d.small, 
      geom=c('point', 'smooth'), 
      method='gam', 
      formula=y ~ s(x, bs='cs'))

# 2.5.2 Boxplots and Jitter
qplot(color, price / carat, data=diamonds, geom='jitter', alpha=I(1 / 20))      
qplot(color, price / carat, data=diamonds, geom='boxplot', alpha=I(1 / 10))


