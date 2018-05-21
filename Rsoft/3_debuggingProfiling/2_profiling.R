#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/3_debuggingProfiling/')

#library(dlmn)
library(dplyr)
library(ggplot2)
library(magrittr)
library(microbenchmark)
load('~/Learning/R/Rsoft/data/chicagoNMMAPS.rda')

error.if.n.greater.than.0 <- function(n) {
  check.n.value(n)
  n
}

#debug(lm)
#debugonce(lm)

options(error=recover)

error.if.n.greater.than.0(5)


# microbenchmark
microbenchmark(a <- rnorm(1000), b <- mean(rnorm(1000)))

date <- c('2015-07-01', '2015-07-02', '2015-07-03', '2015-07-04', '2015-07-05', 
          '2015-07-06', '2015-07-07', '2015-07-08')
temp <- c(26.5, 27.2, 28.0, 26.9, 27.5, 25.9, 28.0, 28.2)
#record.temp <- c(F, T, T, F, F, F, T, T)
data <- data.frame(date=date, temp=temp)

find.records.naive <- function(df, threshold) {
  highest.temp <- c()
  record.temp <- c()
  for (i in 1:nrow(df)) {
    highest.temp <- max(highest.temp, df$temp[i])
    record.temp[i] <- df$temp[i] >= threshold & df$temp[i] >= highest.temp
  }
  df <- cbind(df, record.temp)
  df
}

find.records <- function(df, threshold) {
  temp.cmax <- cummax(df$temp)
  record.temp <- df$temp >= temp.cmax & df$temp > threshold
  cbind(df, record.temp)
}

find.records.tidy <- function(df, threshold) {
  df <- df %>%
    mutate_(over_threshold=~ temp >= threshold, 
            cummax_temp=~ temp == cummax(temp),
            record_temp=~ over_threshold & cummax_temp) %>%
    select_(.dots=c('-over_threshold', '-cummax_temp'))
  return(as.data.frame(df))
}


find.records.naive(data, 27)
find.records(data, 27)
find.records.tidy(data, 27)

record.temp.perf <- microbenchmark(find.records.naive(data, 27),
                                   find.records.tidy(data, 27),
                                   find.records(data, 27))
record.temp.perf

record.temp.perf.big <- microbenchmark(find.records.naive(chicagoNMMAPS, 27),
                                       find.records.tidy(chicagoNMMAPS, 27),
                                       find.records(chicagoNMMAPS, 27))
record.temp.perf.big
autoplot(record.temp.perf.big)