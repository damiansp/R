#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

library(MVA)
data('USairpollution')



# 2. Scatterplot
plot(manu ~ popul, 
     data=USairpollution, 
     ylab='Manufacturing Enterprises ≥ 20 people',
     xlab='Populations (thousands)')
rug(USairpollution$manu, side=2)
rug(USairpollution$popul, side=1)

# 2.1 Bivariate Boxplot
outcity <- match(lab <- c('Chicago', 'Detroit', 'Cleveland', 'Philadelphia'),
                 rownames(USairpollution))
x <- USairpollution[, c('manu', 'popul')]
bvbox(x, 
      mtitle='', 
      ylab='Manufacturing Enterprises ≥ 20 people',
      xlab='Populations (thousands)')
text(x$manu[outcity], 
     x$popul[outcity], 
     labels=lab, 
     cex=0.7, 
     pos=c(2, 2, 4, 2))

# 2.2 Convex hull of bivariate data
hull <- chull(USairpollution$popul, USairpollution$manu)
hull
plot(manu ~ popul, data=USairpollution)
polygon(
  USairpollution$popul[hull], USairpollution$manu[hull], col=rgb(0, 1, 0, 0.5))
cor(USairpollution$manu, USairpollution$popul)

# "trimmed" correlation  
cor(USairpollution$manu[-hull], USairpollution$popul[-hull])

# 2.3 Chi-plot
par(mfrow=c(1, 2))
plot(USairpollution$manu ~ USairpollution$popul)

# if independent ~95% of points should be in central band
chiplot(USairpollution$popul, USairpollution$manu) 



# 3. Bubble and Other Glyph Plots
par(mfrow=c(1, 1))
plot(wind ~ temp, data=USairpollution, ylim=range(wind) * c(0.95, 1.05))
symbols(USairpollution$temp, 
        USairpollution$wind, 
        circles=USairpollution$SO2, 
        add=T, 
        bg=rgb(0, 0, 1, 0.5))
head(USairpollution)
plot(wind ~ temp, data=USairpollution, ylim=range(wind) * c(0.95, 1.05))
stars(USairpollution[, -c(2, 5)], 
      locations=USairpollution[, c(2, 5)], 
      add=T, 
      labels=NULL)
stars(USairpollution)