#-------10#-------20#-------30#-------40#-------50#-------60#-------70#-------80
rm(list=ls())
setwd('~/Learning/R/Rsoft/4_OOP')

library(dplyr)
library(readr)
library(magrittr)
library(reshape)
library(tidyr)

setClass('Subject',
         slots=list(id='numeric',
                    data='data.frame'))
setGeneric(
  'summary',
  function(x) {
  	standardGeneric('summary')
  })
setMethod(
  'summary',
  c(x='Subject'),
  function(x) {
    print.summary <- function() {
      cat(sprintf('ID: %d\n%s', x@id, capture.output(x@data)))	
    }
    capture.output(print.summary())
  })

setClass('LongitudinalData',
         slots=list(dataframe='data.frame',
                    pivot='data.frame'))
         
         
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
  	cat(sprintf('Subject ID: %d\n', x@id))
  })
  
setGeneric(
  'subject',
  function(x, idn) {
  	standardGeneric('subject')
  })
setMethod(
  'subject',
  c(x='LongitudinalData', idn='numeric'),
  function(x, idn) {
  	if (!idn %in% x@pivot$id) {
  	 return (NULL)
  	}
  	subject.table <- subset(x@pivot, id == idn)
  	# Keep columns only if at least one NA value
    keep <- apply(test, 
                  2, 
                  function(x) ifelse(length(x) == sum(is.na(x)), F, T))
    data <- subject.table[, keep]
    new('Subject', id=idn, data=data[, -which(names(data) == 'id')])
  })
  	
         
make_LD <- function(df) {
  pivot <- data %>%
    group_by(id, visit, room) %>%
    summarize(mean.value = mean(value)) %>%
    spread(room, mean.value) %>%
    as.data.frame()
  new('LongitudinalData', dataframe=df, pivot=pivot)
}



# Test code------------------------------------------------------------
data <- read_csv("../data/MIE.csv")
head(data)
x <- make_LD(data)
print(class(x))
print(x)

head(x@dataframe)
head(x@pivot)

test <- subset(x@pivot, id == 106)
test

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
