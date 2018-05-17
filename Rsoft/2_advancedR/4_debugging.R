#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/2_advancedR/')


# traceback()
check.n.value <- function(n) {
  if (n > 0) {
  	stop('n should be <= 0')
  }
}

error.if.n.greater.than.0 <- function(n) {
  check.n.value(n)
  n
}

error.if.n.greater.than.0(5)
traceback()


# browsing
check.n.value <- function(n) {
  if (n > 0) {
    browser()
    stop('n should be <= 0')
  }
}

error.if.n.greater.than.0(5)



# tracing
trace('check.n.value')
error.if.n.greater.than.0(5)

as.list(body(check.n.value))
as.list(body(check.n.value)[[2]])

trace('check.n.value', browser, at=list(c(2, 3)))
check.n.value
body(check.n.value)

trace(
  'check.n.value', 
  quote(
    if (n == 5) {
      message('invoking the browser')
      browser()
    }),
  at=2)

body(check.n.value)

trace('glm', browser, at=4, where=asNamespace('stats'))
body(stats::glm)[1:5]