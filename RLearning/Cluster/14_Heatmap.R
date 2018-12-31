#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')

library(d3heatmap)
library(gplots)
library(pheatmap)
library(RColorBrewer)

# 2. Data Prep
df <- scale(mtcars)
head(df)



# 3. R Base Heatmap: heatmap()
heatmap(df, scale='none')

col <- colorRampPalette(c('green', 'grey', 'red'))(256)
heatmap(df, scale='none', col=col)

col <- colorRampPalette(brewer.pal(10, 'RdYlBu'))(256)
heatmap(df, 
        scale='none', 
        col=col, 
        RowSideColors=rep(c('cadetblue', 'coral'), each=16),
        ColSideColors=c(rep('darkblue', 5), rep('darkorange4', 6)))


        
# 4. Enhanced Heat Maps: heatmap.2()
heatmap.2(df, scale='none', col=bluered(100), trace='none')



# 5. Pretty Heat Maps: pheatmap()
pheatmap(df, cutree_rows=4, cutree_cols=3)



# 6. Interactive Heat Maps: d3heatmap()
d3heatmap(scale(mtcars), colors='RdYlBu', k_row=4, k_col=3)