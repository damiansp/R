#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

# library(dextend) # no 3.5 version
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



# 2 Case of Dendogram with Large Data Sets


# 2.1 Zooming in to the Dendrogram
fviz_dend(hc, xlim=c(1, 29), ylim=c(-1, 3.1))


# 2.2 Plotting a Subtree of Dendrograms
dend.plot <- fviz_dend(hc, k=4, cex=0.5, k_colors='jco')
dend.data <- attr(dend.plot, 'dendrogram')
dend.cuts <- cut(dend.data, h=2.5)
fviz_dend((dend.cuts$upper))
dend.plot
#print(dend.plot) # same

fviz_dend(dend.cuts$lower[[1]], main='Subtree 1')
fviz_dend(dend.cuts$lower[[3]], main='Subtree 3')
fviz_dend(dend.cuts$lower[[4]], main='Subtree 4')
fviz_dend(dend.cuts$lower[[4]], type='circular')


# 2.3 Saving Dendrogram Images
pdf('dendrogram.pdf', width=30, height=15)
p <- fviz_dend(hc, k=4, cex=1, k_colors='jco')
print(p)
dev.off()



# 3. Manipulating Dendrograms with Dendextend
dat <- scale(USArrests)
dist.res <- dist(dat)
hc <- hclust(dist.res, method='ward.D2')
dend <- as.dendrogram(hc)
plot(dend)

dend <- USArrests[1:5, ] %>% 
  scale %>% 
  dist %>% 
  hclust(method='ward.D2') %>% 
  as.dendrogram
plot(dend)

# dextend:
mycols <- c('#2e9fdf', '#00afbb', '#e7b800', '#fc4e07')
dend <- as.dendrogram(hc) %>%
  set('branches_lwd', 1) %>%
  set('branches_k_color', mcols, k=4) %>%
  set('labels_colors', mycols, k=4) %>%
  set('labels_cex', 0.5)
fviz_dend(dend)



