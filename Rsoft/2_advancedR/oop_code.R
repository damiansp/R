rm(list=ls())
setwd('~/Learning/R/Rsoft/')

library(dplyr)
library(readr)
library(magrittr)
library(tidyr)



# A class that coerces print() to use cat() instead
setClass('Cat', slots=list(text='character'))       
setClass('Summary', slots=list(output='function'))
setClass(
  'LongitudinalData', slots=list(data='data.frame', pivot='data.frame'))
setClass('Subject', slots=list(id='numeric'), contains='LongitudinalData')
setClass('Visit', slots=list(visit='numeric'), contains='Subject')
setClass('Room', slots=list(room='character'), contains='Visit')



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
  	pivot <- subject.table[, keep]
  	data <- subset(long.dat@data, id == idn)
  	new('Subject', id=idn, pivot=pivot, data=data)
  })
  
  
setGeneric(
  'visit',
  function(x, visit.number) {
    standardGeneric('visit')
  })
setMethod(
  'visit',
  c(x='Subject', visit.number='numeric'),
  function(x, visit.number) {
  	if (!visit.number %in% x@data$visit) {
  	  return (NULL)
  	}
  	new('Visit', 
  	    id=x@id, 
  	    visit=visit.number, 
  	    data=subset(x@data, visit == visit.number),
  	    pivot=subset(x@pivot, visit == visit.number))
  })
  

setGeneric(
  'room',
  function(x, room.name) {
  	standardGeneric('room')
  })
setMethod(
  'room',
  c(x='Subject', room.name='character'),
  function(x, room.name) {
    new('Room', 
        id=x@id, 
        room=room.name, 
        visit=x@visit, 
        data=subset(x@data, room == room.name),
        pivot=x@pivot[, which(names(x@pivot) %in% c('id', 'visit', room.name))])
  })
  

# Summary methods------------------------------------------------------
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
      x@pivot
    }
    new('Summary', output=f)
  })
setMethod(
  'summary',
  c(x='Room'),
  function(x) {
  	f <- function() {
  	  data <- as.data.frame(x@data)[, 'value']
  	  qs <- quantile(data, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=T)
  	  avg <- mean(data, na.rm=T)
  	  cat(sprintf('ID: %d\n', x@id))
  	  cat(sprintf('%7s %7s %7s %7s %7s %7s\n', 
  	              'Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.'))
  	  cat(sprintf('%7.2f %7.2f %7.2f %7.2f %7.2f %7.2f',
  	              qs[1], qs[2], qs[3], avg, qs[4], qs[5]))
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
  	        length(unique(x@data$id)))
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
setMethod(
  'print',
  c(x='Room'),
  function(x) {
    room.text <- sprintf('ID: %d\nVisit: %d\nRoom: %s', x@id, x@visit, x@room)
    cat.text <- new('Cat', text=room.text)
    print(cat.text)
  })


make_LD <- function(df) {
  pivot <- data %>%
    group_by(id, visit, room) %>%
    summarize(mean.value = mean(value)) %>%
    spread(room, mean.value) %>%
    as.data.frame()
  new('LongitudinalData', data=df, pivot=pivot)
}




# Test code------------------------------------------------------------
#data <- read_csv("data/MIE.csv")
#head(data)
#x <- make_LD(data)
#print(class(x))
#print(x)

#head(x@data)
#head(x@pivot)

#test <- subset(x@pivot, id == 106)
#test

## Subject 10 doesn't exist
#out <- subject(x, 10)
#print(out)

#out <- subject(x, 14)
#print(out)

#out <- subject(x, 54) %>% summary
#print(out)

#out <- subject(x, 14) %>% summary
#print(out)

#out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
#print(out)

## Show a summary of the pollutant values
#out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
#print(out)

#out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
#print(out)

