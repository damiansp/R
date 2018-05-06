#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/')

library(data.table)
library(dplyr)


brazil.zika <- fread('./data/COES_Microcephaly-2016-06-25.csv')
head(brazil.zika)
class(brazil.zika)

fread('./data/COES_Microcephaly-2016-06-25.csv', 
      select=c('location', 'value', 'unit')) %>%
  slice(1:3)      