rm(list=ls())
library(purrr)


# Expressions
two.plus.two <- quote(2 + 2)
two.plus.two
eval(two.plus.two)    # 4
deparse(two.plus.two) # '2 + 2'

exp.string <- '2 + 2'
expr <- parse(text=exp.string) # expr same as expression('2 + 2')
eval(expr)                     # 4

sum.expr <- quote(sum(1, 5)) # sum(1, 5)
eval(sum.expr)               # 6
sum.expr[[1]]                # sum
sum.expr[[2]]                # 1
sum.expr[[3]]                # 5
sum.expr[[1]] <- quote(paste0)
eval(sum.expr)               # '15'

sum40.50expr <- call('sum', 40, 50) # same as quote(sum(40, 50))
sum40.50expr
eval(sum40.50expr) # 90

return.expression <- function(...) {
  match.call() # get expression from user
}

return.expression(2, col='blue', F)


first.arg <- function(...) {
  expr <- match.call()
  first.arg.expr <- expr[[2]]
  first.arg <- eval(first.arg.expr)
  if (is.numeric(first.arg)) {
    paste('The first arg is', first.arg)
  } else {
  	'The first arg is not numeric'
  }
}

first.arg(2, 4, 'string', F)
first.arg('two', 4, 'string', T)



# Environments
my.env <- new.env()
my.env$x <- 4
my.env$x

assign('y', 9, envir=my.env) # same as my.env$y <- 9
get('y', envir=my.env)
my.env$y

ls() # global environment
ls(my.env)
rm(y, envir=my.env)
exists('y', envir=my.env) # F
exists('x', envir=my.env) # T
my.env$x
my.env$y # NULL

search()



# Execution Environments
x <- 10
my.func <- function() {
  x <- 5
  x
}
my.func() # 5

global.func <- function() {
  x
}
global.func() # 10

assign.to.global <- function() {
  x <<- 'wow'
}

assign.to.global()
x # 'wow'

unassigned.var # Error - does not exist
exists('unassigned.var') # F

assign.new.var <- function() {
  unassigned.var <<- 'Magic!'
}

assign.new.var()
exists('unassigned.var') # T
unassigned.var # 'Magic!'


