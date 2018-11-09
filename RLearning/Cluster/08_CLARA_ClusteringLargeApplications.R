#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Cluster')

library(cluster)
library(factoextra)

#data(USArrests)



# 3. Computing CLARA in R
# 3.1 Data Format and Prep
df <- rbind(cbind(rnorm(200,  0,  8), rnorm(200,  0,  8)),
            cbind(rnorm(200, 50,  8), rnorm(200, 50,  8)),
            cbind(rnorm(200, 75, 10), rnorm(200, 75, 10)))
df <- data.frame(df)
colnames(df) <- c('x', 'y')
df$cluster <- rep(c(1, 2, 3), each=200)
rownames(df) <- paste('S', 1:nrow(df), sep='')
head(df)

