#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/2_advancedR')

library(microbenchmark)

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



# Handling Errors
catcher <- function(expr) {
  tryCatch(expr, 
           error=function(e) { message('An error occurred:\n', e) },
           warning=function(w) { warning('This is only a warning:\n', w) },
           finally=message('Finally done!'))
}

catcher(2 + 2)
catcher('two' + 2)
catcher(as.numeric(c(1, 'two', 3)))

is.even <- function(n) {
  tryCatch(n %% 2 == 0, error=function(e) { F })
}

is.even(714)
is.even('eight')

# But faster to do
is.even2 <- function(n) {
  is.numeric(n) && n %% 2 == 0
}

microbenchmark(sapply(letters, is.even))
microbenchmark(sapply(letters, is.even2))