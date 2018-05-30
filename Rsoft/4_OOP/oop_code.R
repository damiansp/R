#-------10#-------20#-------30#-------40#-------50#-------60#-------70#-------80
rm(list=ls())
setwd('~/Learning/R/Rsoft/4_OOP')

library(dplyr)
library(readr)
library(magrittr)
library(tidyr)



# A class that coerces print() to use cat() instead
setClass('Cat', slots=list(text='character'))
         
setClass('LongitudinalData', 
         slots=list(dataframe='data.frame', pivot='data.frame'))

setClass('Subject', slots=list(id='numeric', data='data.frame'))

setClass('Summary', slots=list(output='function'))



setGeneric(
  'subject',
  function(long.dat, idn) {
  	standardGeneric('subject')
  })
setMethod(
  'subject',
  c(long.dat='LongitudinalData', idn='numeric'),
  function(long.dat, idn) {
  	if (!idn %in% long.dat@pivot$id) {
  	  return (NULL)
  	}
  	subject.table <- subset(long.dat@pivot, id == idn)
  	# Keep only columns w at least one non-NA value
  	keep <- apply(subject.table,
  	              2,
  	              function(x) ifelse(length(x) == sum(is.na(x)), F, T))
  	data <- subject.table[, keep]
  	new('Subject', id=idn, data=data)
  })
  
  
setGeneric(
  'summary',
  function(x) {
  	standardGeneric('summary')
  })
setMethod(
  'summary',
  c(x='Subject'),
  function(x) {
  	f <- function() {
      cat(sprintf('ID: %d\n', x@id))
      x@data
    }
    new('Summary', output=f)
  })
  

# Print funcs----------------------------------------------------------
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
  	sprintf('Subject ID: %d', x@id)
  })  	
setMethod(
  'print',
  c(x='Cat'),
  function(x) {
  	cat(x@text)
  })
setMethod(
  'print',
  c(x='Summary'),
  function(x) {
  	x@output()
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

a <- 'the'
b <- data.frame(a=1, b=2, c=3)
cat(sprintf('%s\n%s', a, capture.output(b)))
test <- function() {
  cat(sprintf('%s\n', a))
  b
}
test()