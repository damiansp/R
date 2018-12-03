#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning')

library(corrplot)
library(dendextend)


# 1. Data Prep
df <- scale(USArrests)
ss <- sample(50, 10)
df <- df[ss, ]



# 2. Comparing Dendrograms
res.dist <- dist(df, method='euclidean')
hc1 <- hclust(res.dist, method='average')
hc2 <- hclust(res.dist, method='ward.D2')
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend.list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2)
tanglegram(dend1, 
           dend2, 
           highlight_distinct_edges=F,
           #common_subtrees_color_lines=F,
           common_subtrees_color_branches=T,
           main=paste('Entanglement =', round(entanglement(dend.list), 2)))
           
# Corr matrix between dendrograms
# Cophenetic corr matrix
cor.dendlist(dend.list, method='cophenetic')

# Baker corr mat
cor.dendlist(dend.list, method='baker')

# Same as:
cor_cophenetic(dend1, dend2)
cor_bakers_gamma(dend1, dend2)

make.dend <- function(df, method) {
  df %>% dist %>% hclust(method) %>% as.dendrogram	
}

d1 <- make.dend(df, 'complete')
d2 <- make.dend(df, 'single')
d3 <- make.dend(df, 'average')
d4 <- make.dend(df, 'centroid')
dend.list <- dendlist('Complete'=d1, 'Single'=d2, 'Average'=d3, 'Centroid'=d4)
cors <- cor.dendlist(dend.list)
round(cors, 2)
par(mfrow=c(1, 1))
corrplot(cors, method='color', type='lower')