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


# 2.5.3 Histograms; Density Plots
qplot(carat, data=diamonds, geom='histogram')
qplot(carat, 
      data=diamonds, 
      geom='histogram', 
      binwidth=0.1, 
      xlim=c(0, 3), 
      fill=color)
qplot(carat, data=diamonds, geom='density')
qplot(carat, data=diamonds, geom='density', color=color)


# 2.5.4 Bar Charts
qplot(color, data=diamonds, geom='bar')
qplot(color, data=diamonds, geom='bar', weight=carat) +  
  scale_y_continuous('carat')
  
qplot(date, unemploy / pop, data=economics, geom='line')
qplot(date, uempmed, data=economics, geom='line')

year <- function(x) { as.POSIXlt(x)$year + 1900 }
qplot(unemploy / pop, uempmed, data=economics, geom=c('point', 'path'))
qplot(unemploy / pop, uempmed, data=economics, geom='path', color=year(date))


# 2.6 Faceting
qplot(carat, 
      data=diamonds, 
      facets=color ~ ., 
      geom='histogram', 
      binwidth=0.1, 
      xlim=c(0, 3))
qplot(carat, 
      ..density..,
      data=diamonds, 
      facets=color ~ ., 
      geom='histogram', 
      binwidth=0.1, 
      xlim=c(0, 3))      