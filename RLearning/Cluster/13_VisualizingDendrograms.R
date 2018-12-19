#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(factoextra)
library(igraph)
data(USArrests)

dd <- dist(scale(USArrests), method='euclidean')
hc <- hclust(dd, method='average')


# 1. Visualizing Dendrograms
PALETTE = c('#2e9fdf', '#00afbb', '#e7b800', '#fc4e07', '#660088')
fviz_dend(hc, cex=0.5)
fviz_dend(hc, cex=0.5, main='State Crime: Average Linkage', ylab='Distance')
fviz_dend(hc, cex=0.5, main='State Crime: Average Linkage', horiz=T)
fviz_dend(hc,
          k=5,
          cex=0.5,
          k_colors=PALETTE,
          color_labels_by_k=F,
          rect=T,
          rect_border=PALETTE,
          rect_fill=T)
fviz_dend(hc,
          k=5,
          cex=0.5,
          k_colors=PALETTE,
          color_labels_by_k=F,
          rect=T,
          rect_border=PALETTE,
          rect_fill=T,
          ggtheme=theme_grey())
fviz_dend(hc,
          k=5,
          cex=0.5,
          k_colors='jco') 
          # alt cols={npg, aaas, lancet, ucscgb, uchicago, simpsons, rickandmorty}
fviz_dend(hc,
          cex=0.5,
          k=5,
          k_colors='simpsons',
          type='circular')
fviz_dend(hc,
          k=5,
          k_colors='jco',
          type='phylogenic',
          repel=T)
fviz_dend(hc,
          k=5,
          k_colors='jco',
          type='phylogenic',
          repel=T,
          phylo_layout='layout.gem')