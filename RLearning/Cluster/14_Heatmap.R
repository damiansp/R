#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Cluster')


# 2. Data Prep
df <- scale(mtcars)
head(df)



# 3. R Base Heatmap: heatmap()
heatmap(df, scale='none')
