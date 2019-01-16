#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')
#source('https://bioconductor.org/biocLite.R')
#biocLite('ComplexHeatmap')
library(circlize)
library(cluster)
library(ComplexHeatmap)
library(d3heatmap)
library(dendextend)
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



# 7. Enhancing Heat Maps Using dendextend
rowv <- mtcars %>% 
  scale %>% 
  dist %>% 
  hclust %>% 
  as.dendrogram %>% 
  set('branches_k_color', k=4) %>% 
  set('branches_lwd', 1.2) %>% 
  ladderize
colv <- mtcars %>%
  scale %>%
  t %>%
  dist %>%
  hclust %>%
  as.dendrogram %>%
  set('branches_k_color', k=3, , value=c('orange', 'blue')) %>%
  set('branches_lwd', 1.2) %>%
  ladderize
heatmap(scale(mtcars), Rowv=rowv, Colv=colv, scale='none')
heatmap.2(scale(mtcars), 
          scale='none', 
          col=bluered(100), 
          Rowv=rowv, 
          Colv=colv, 
          trace='none', 
          density.info='none')
d3heatmap(scale(mtcars), colors='RdBu', Rowv=rowv, Colv=colv)



# 8 Complex Heat Map


# 8.1 Simple heatmap
Heatmap(df,
        name='mtcars',
        column_title='Variables',
        row_title='Samples',
        row_names_gp=gpar(fontsize=7))
mycols <- colorRamp2(breaks=c(-2, 0, 2), colors=c('green', 'white', 'red'))
Heatmap(df, name='mtcars', col=mycols)
Heatmap(
  df, name='mtcars', col=colorRamp2(c(-2, 0, 2), brewer.pal(n=3, name='RdBu')))

# Splitting Heat Map by Rows
Heatmap(df, name='mtcars', split=mtcars$cyl, row_names_gp=gpar(fontsize=7))
Heatmap(df,
        name='mtcars',
        split=data.frame(cyl=mtcars$cyl, am=mtcars$am),
        row_names_gp=gpar(fontsize=7))
        
pa <- pam(df, k=3)
Heatmap(df, name='mtcars', col=mycols, km=2, split=paste0('pam', pa$clustering))


# 8.3 Heatmap Annotation
df <- t(df)

# 8.3.1 Simple Annotation
annot.df <- data.frame(cyl=mtcars$cyl, am=mtcars$am, mpg=mtcars$mpg)
col <- list(cyl=c('4'='green', '6'='grey', '8'='darkred'),
            am=c('0'='yellow', '1'='orange'),
            mpg=circlize::colorRamp2(c(17, 25), c('lightblue', 'purple')))
ha <- HeatmapAnnotation(annot.df, col=col)
Heatmap(df, name='mtcars', top_annotation=ha)



# 9 Application: Gene Expression Matrix
expr <- readRDS(paste0(system.file(package='ComplexHeatmap'), 
                       '/extdata/gene_expression.rds'))
mat <- as.matrix(expr[, grep('cell', colnames(expr))])
type <- gsub('s\\d+_', '', colnames(mat))
ha <- HeatmapAnnotation(df=data.frame(type=type))
Heatmap(mat, 
        name='gene expression', 
        km=5, 
        top_annotation=ha, 
        top_annotation_height=unit(4, 'mm'), 
        show_row_names=F, show_column_names=F) +
Heatmap(expr$length, 
        name='length', 
        width=unit(5, 'mm'), 
        col=circlize::colorRamp2(c(0, 100000), 
        c('white', 'orange'))) +
Heatmap(expr$type, name='type', width=unit(5, 'mm')) +
Heatmap(expr$chr, 
        name='chr', 
        width=unit(5, 'mm'), 
        col=circlize::rand_color(length(unique(expr$chr))))
    
    
        
# 10. Visualizing the Distribution of Columns in Matrix
densityHeatmap(scale(mtcars))