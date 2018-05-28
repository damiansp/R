#!/usr/bin/env Rscript
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
factorial.vectorize <- function(n) {
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
  cat('Testing factorial functions output correct values:\n')
  cat('\nloop----------------------------\n')
  cat(unlist(lapply(n, factorial.loop)))
  cat('\n\nreduce--------------------------\n')
  cat(unlist(lapply(n, factorial.reduce)))	
  cat('\n\nrecursive-----------------------\n')
  cat(unlist(lapply(n, factorial.recursive)))	
  cat('\n\nvectorize-----------------------\n')
  cat(unlist(lapply(n, factorial.vectorize)))	
  cat('\n\nmemoize--------------------------\n')
  cat(unlist(lapply(n, factorial.memoize)))	
}

test()


# Run benchmark tests
run.benchmarks <- function(n) {
  if (length(n) > 1) {
    benchmarks <- microbenchmark(lapply(n, factorial.loop),
                                 lapply(n, factorial.reduce),
                                 lapply(n, factorial.recursive),
                                 lapply(n, factorial.vectorize),
                                 lapply(n, factorial.memoize),
                                 lapply(n, factorial)) # <- r built-in
  } else {
    benchmarks <- microbenchmark(factorial.loop(n),
                                 factorial.reduce(n),
                                 factorial.recursive(n),
                                 factorial.vectorize(n),
                                 factorial.memoize(n),
                                 factorial(n)) # <- r built-in	
  }
  benchmarks
}


cat('\n\n\nRunning Benchmarks for batches of 10 numbers at a time:')
cat('\nBenchmarks for n! for.....')
for (i in c(10, 20, 40, 80, 160)) {
  cat(sprintf('\n\n  n = %d, ..., %d ----------------------------\n', i - 10, i))
  ns <- (i - 10):i
  benchmarks <- run.benchmarks(ns)
  print(benchmarks)
  autoplot(benchmarks)
}
