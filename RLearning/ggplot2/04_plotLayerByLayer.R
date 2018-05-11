rm(list=ls())
setwd('~/Learning/R/RLearning/ggplot2/')

library(ggplot2)
data(diamonds)

p <- ggplot(diamonds, aes(x=carat))
p <- p + layer(geom='bar', 
               geom_params=list(fill='steelblue'), 
               stat='bin', 
               stat_params=list(binwidth=2))
print(p)