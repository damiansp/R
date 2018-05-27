#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/4_OOP')

library(ggplot2)
library(magrittr)
library(microbenchmark)
library(purrr)


# Part 1. Factorial
# Loop
factorial.loop <- function(n) {
  if (n == 0) {
  	return (1)
  }
  product <- 1
  for (i in 1:n) {
  	product <- product * i
  }
  product
}


# Reduce
factorial.reduce <- function(n) {
  if (n == 0) {
  	return (1)
  }
  if (n > 12) {
  	warning(paste('"reduce" suffers from overflow with n > 12.',
  	              'try a more efficient function', sep='\n'))
  	return (NA)
  }
  1:n %>% reduce(`*`)
}


# Recursive
factorial.recursive <- function(n) {
  if (n == 1 | n == 0) {
  	return (1)
  } else {
  	return (n * factorial.recursive(n - 1))
  }
}


# Vectorized
factorial.vector <- function(n) {
  if (n == 0) {
  	return (1)
  }
  return (cumprod(1:n)[n])
}


# Memoize
MAX_FACTORIAL <- 100
factorial.table <- c(1, rep(NA, MAX_FACTORIAL))
factorial.memoize <- function(n) {
  if (!is.na(factorial.table[n + 1])) {
  	return (factorial.table[n + 1])
  }
  factorial.table[n] <<- factorial.memoize(n - 1)
  factorial.table[n] * n
}



# Test that functions output correct values
test <- function() {
  n <- 0:6
  cat('Testing factorial functions:\n')
  cat('loop----------------------------\n')
  cat(unlist(lapply(n, factorial.loop)))
  cat('\nreduce--------------------------\n')
  cat(unlist(lapply(n, factorial.reduce)))	
  cat('\nrecursive-----------------------\n')
  cat(unlist(lapply(n, factorial.recursive)))	
  cat('\nvector--------------------------\n')
  cat(unlist(lapply(n, factorial.vector)))	
  cat('\nmemoize--------------------------\n')
  cat(unlist(lapply(n, factorial.memoize)))	
}

test()


# Run benchmark tests
run.benchmarks <- function(n) {
  if (length(n) > 1) {
    benchmarks <- microbenchmark(lapply(n, factorial.loop),
                                 lapply(n, factorial.reduce),
                                 lapply(n, factorial.recursive),
                                 lapply(n, factorial.vector),
                                 lapply(n, factorial.memoize))
  } else {
    benchmarks <- microbenchmark(factorial.loop(n),
                                 factorial.reduce(n),
                                 factorial.recursive(n),
                                 factorial.vector(n),
                                 factorial.memoize(n)) 	
  }
  benchmarks
}

# Small numbers
ns <- 0:10
benchmarks <- run.benchmarks(ns)
autoplot(benchmarks)


# Bigger numbers
n <- 20
benchmarks <- run.benchmarks(n)
autoplot(benchmarks)

n <- 40
benchmarks <- run.benchmarks(n)
autoplot(benchmarks)

n <- 80
benchmarks <- run.benchmarks(n)
autoplot(benchmarks)

n <- 160
benchmarks <- run.benchmarks(n)
autoplot(benchmarks)