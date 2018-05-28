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



# 4. The Scatterplot Matrix
pairs(USairpollution, pch=16, col=rgb(0, 0, 0, 0.4))
round(cor(USairpollution), 4)
pairs(USairpollution, panel=panel.smooth, pch=16, col=rgb(0, 0, 0, 0.4))
pairs(
  USairpollution, 
  panel=function(x, y, ...) {
  	points(x, y, ...)
  	abline(lm(y ~ x), col=4)
  }, 
  pch=16, 
  col=rgb(0, 0, 0, 0.4))
  
  
  
# 5. Enhancing the Scatterplot with Esimated Bivariate Densities
# 5.1 Kernel density estimators
rec <- function(x) { (abs(x) < 1) / 2 }
tri <- function(x) { (abs(x) < 1) * (1 - abs(x)) }
gauss <- function(x) { (1 / sqrt(2*pi)) * exp(-(x^2) / 2) }
x <- seq(-3, 3, 0.001)
plot(x, rec(x), type='l', ylim=c(0, 1), ylab='K(x)')
lines(x, tri(x), col=2)
lines(x, gauss(x), col=4)
legend('topleft', 
       legend=c('Rectangualar', 'Triangualar', 'Gaussian'), 
       lty=1, 
       col=c(1, 2, 4), 
       title='Kernel Functions', 
       bty='n')

kernel.est.plot <- function(x, h, kernel.func) {
  n <- length(x)
  xgrid <- seq(min(x) - 1, max(x) + 1, 0.01)
  bumps <- sapply(x, function(a) { kernel.func((xgrid - a)/h) / (n*h) })
  plot(xgrid, 
       rowSums(bumps), 
       ylab=expression(hat(f)(x)), 
       type='l', 
       xlab='x', 
       lwd=2,
       col=4)
  rug(x, col=rgb(0, 0, 0, 10 / n))  
  out <- apply(
    bumps, 
    2, 
    function(b) { lines(xgrid, b, col=rgb(0, 0, 0, 10 / n)) })	
}
x <- rnorm(80, 5, 2)
hs <- c(0.2, 0.4, 0.6)
fs <- c(rec, tri, gauss)
par(mfrow=c(3, 3), mar=rep(0.5, 4))
for (h in hs) {
  for (f in fs) {
    kernel.est.plot(x, h, f)
  }
}
