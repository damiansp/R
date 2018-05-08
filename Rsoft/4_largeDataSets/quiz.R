#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/4_largeDataSets')

#library(readr)

spec <- read.csv('../data/daily_SPEC_2014.csv.bz2')
head(spec)
names(spec)

# 1
mean(spec[(spec$State.Name == 'Wisconsin' 
           & spec$Parameter.Name == 'Bromine PM2.5 LC'), 
          c('Arithmetic.Mean')], 
     na.rm=T) 
     
# 2
sort(tapply(spec$Arithmetic.Mean, spec$Parameter.Name, mean, na.rm=T))

# 3
sulfate <- subset(spec, Parameter.Name == 'Sulfate PM2.5 LC')
head(sulfate)