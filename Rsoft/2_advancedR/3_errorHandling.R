#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/2_advancedR')

'hello' + 'world'
as.numeric(c('5', '6', 'seven'))

f <- function() { message('This is a message.') }
f()



# Generating Errors
stop('Something\'s amiss')

erroneous.function <- function() { stop('Somthing\'s amiss') }
erroneous.function()

error.if.nonnegative <- function(n) {
  stopifnot(n <= 0)
  n
}

error.if.nonnegative(-7)
error.if.nonnegative(7)

warning('Consider yourself warned!')

make.NA <- function(x) {
  if (!(is.numeric(x))) { 
  	warning('Coerced to NA')
  	return(NA)
  }
  x 
}

make.NA(7)
make.NA('sodium')

message('in a bottle (yeah)')

