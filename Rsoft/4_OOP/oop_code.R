#-------10#-------20#-------30#-------40#-------50#-------60#-------70#-------80
rm(list=ls())
setwd('~/Learning/R/Rsoft/4_OOP')

#library(dplyr)
library(magrittr)
library(tidyr)

#LongitudinalData <- setRefClass(
#  'LongitudinalData',
#  fields=list(df='data.frame'),
#  methods=list(
#    show = function() {
#      cat(sprintf('Longitudinal data set with %d subjects', 
#                  length(unique(df$id)))),
#    },
#    subject = function() {
#      
#    })
#)

#make_LD <- function(df) {
#  LongitudinalData$new(df=df)
#}

setClass('Subject',
         slots=list(id='numeric',
                    data='matrix'))
setGeneric(
  'summary',
  function(x) {
  	standardGeneric('summary')
  })
setMethod(
  'summary',
  c(x='Subject'),
  function(x) {
  	print(sprintf('ID: %d', x@id))
  	print(head(x$dataframe))
  })

setClass('LongitudinalData',
         slots=list(dataframe='data.frame'))
         
         
setGeneric('print')
setMethod(
  'print',
  c(x='LongitudinalData'),
  function(x) {
  	sprintf('Longitudinal data set with %d subjects', 
  	        length(unique(x@dataframe$id)))
  })
setMethod(
  'print',
  c(x='Subject'),
  function(x) {
  	cat(sprintf('ID: %d\n', x@id))
  	print(x@data)
  })
  
setGeneric(
  'subject',
  function(x, id) {
  	standardGeneric('subject')
  })
setMethod(
  'subject',
  c(x='LongitudinalData'),
  function(x, id) {
    sub <- subset(x@dataframe,  id == id) 
    visit <- sort(unique(sub$visit))
    temp <- list()
    for (v in visit) {
      ssub <- subset(sub, visit == v)
      temp[[v + 1]] <- tapply(ssub$value, ssub$room, mean)
    }
    m <- temp[[1]]
    for (i in 2:length(temp)) {
      m <- rbind(m, temp[[i]])
    }
    m <- cbind(visit, m)
    rownames(m) <- rep('', length(visit))
	new('Subject', id=id, data=m)
  })
         
make_LD <- function(df) {
  new('LongitudinalData', dataframe=df)
}



data <- read.csv('../data/MIE.csv')
head(data)
x <- make_LD(data)
class(x)
print(x)
out <- subject(x, 14)
print(out)
s <- subset(data, id==14 & visit==0)
head(s)
mean(s$value)

    sub <- subset(data,  id == id) 
    head(sub)
    visit <- sort(unique(sub$visit))
    temp <- list()
    for (v in visit) {
      ssub <- subset(sub, visit == v)
      head(ssub)
      temp[[v + 1]] <- tapply(ssub$value, ssub$room, mean)
    }
    m <- temp[[1]]
    for (i in 2:length(temp)) {
      m <- rbind(m, temp[[i]])
    }
    m <- cbind(visit, m)
    rownames(m) <- rep('', length(visit))

mean(subset(data, id == 14 & visit == 0)$value)