#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/Multivariate')

#library(MVA)
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