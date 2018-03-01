rm(list = ls())
library(tseries)

START_YEAR <- 2007
END_YEAR <- 2017
START_MONTH <- 1
FREQ <- 12
N_TEST <- 12

load.data <- function(path, log.data) {
  dat <- read.csv(path)
  dat$month <- as.Date(dat$month, format='%m/%d/%y')
  names(dat) <- c('month', 'rev')
  if (log.data) {
    cat('Converting values to log(values)\n')
    dat$rev <- log(dat$rev)
  }
  cat('DF Head:\n')
  print(head(dat))
  cat('DF Tail:\n')
  print(tail(dat))

  cat('\nrows:', nrow(dat))
  dat <- ts(dat$rev, start=c(START_YEAR, START_MONTH), freq=FREQ)
  dat
}

get.rmse <- function(preds, targets) {
  sqrt(mean((preds - targets)^2))
}

add.years.months <- function(init, add) {
  # Takes <init> date in form c(YEAR, MONTH) and adds c(YEARS, MONTHS)
  new <- init + add
  year <- new[1]
  month <- new[2]
  
  if (month > 12) {
    quotient <- trunc(month / 12)
    remainder <- month %% 12
    new <- c(year + quotient, remainder)
  }
  
  new
}

periods.to.years.months <- function(n.periods) {
  years <- trunc(n.periods / 12)
  months <- n.periods %% 12
  c(years, months)  
}

slice.ts <- function(
  t.series, start.idx, end.idx, ts.start=c(START_YEAR, START_MONTH)) {
	  
  start.date <- periods.to.years.months(start.idx)
  start.date <- add.years.months(c(START_YEAR, START_MONTH), start.date)
  start.date <- add.years.months(start.date, c(0, -1))
  end.date   <- periods.to.years.months(end.idx)
  end.date   <- add.years.months(c(START_YEAR, START_MONTH), end.date)
  end.date   <- add.years.months(end.date, c(0, -1))
  ts(t.series[start.idx:end.idx], start=start.date, end=end.date, freq=FREQ)
}

split.train.test <- function(dat, n.test=N_TEST) {
  n <- length(dat)
  n.train <- n - n.test
  train <- slice.ts(dat, 1, n.train)
  test  <- slice.ts(dat, n.train + 1, n)
  list(test=test, train=train)
}

update.best <- function(best, rmse, mod.name) {
  if (rmse < best$rmse) { 
    best$rmse <- rmse
    best$mod <- mod.name
  }

  best
}



dat <- load.data('~/repos/scripts-analytics/revenue/2018/revTS.csv', log.data=T)
split <- split.train.test(dat)
train <- split$train
test  <- split$test

plot(train, xlim=c(2007, 2018), ylim=range(c(train, test)))
lines(test, col=2)
abline(v=2018, col=4)


# Holt-Winters Models--------------------------------------------------------
baseline.hw.mod <- HoltWinters(train, beta=0, gamma=0, seasonal='additive')
baseline.hw.pred <- predict(baseline.hw.mod, n.ahead=N_TEST)

plot.mod <- function(mod.fitted, mod.preds, mod.name) {
  plot(mod.fitted, 
       lwd=2,
       xlim=c(START_YEAR, END_YEAR + 1), 
       ylim=range(train, test),
       main=mod.name)
  lines(test, col='grey', lwd=2)
  lines(mod.preds, col=2, lwd=2)
  legend('topleft', 
         lwd=c(1, 2, 2), 
         col=c('black', 'grey', 'red'), 
         legend=c('train', 'test', 'model'))
}

plot.mod(baseline.hw.mod, baseline.hw.pred, 'HW Baseline')
(baseline.hw.rmse <- get.rmse(baseline.hw.pred, test)) 
best <- list(rmse=baseline.hw.rmse, mod=baseline.hw.mod)

find.best.hw <- function(train.set, iters=10000) {
  best.HW.sse <- Inf
  best.HW.params <- c(0, 0)
  sses <- rep(NA, iters)

  for (i in 1:iters) {
    if (i %% 500 == 0) { 
      cat('Iteration:', i, '\n') 
    }

    beta <- runif(1)
    gamma <- runif(1)
    hw.test.mod <- HoltWinters(
      train.set, beta=beta, gamma=gamma, seasonal='additive')

    if (hw.test.mod$SSE < best.HW.sse) {
      best.HW.params <- c(beta, gamma)
      best.HW.sse <- hw.test.mod$SSE
      best.HW.mod <- hw.test.mod
    }
    sses[i] <- best.HW.sse
  }

  plot(sses, xlab='Iterations', ylab='SSE of best model found', type='l')
  best.HW.mod
}

best.hw.mod <- find.best.hw(train)
best.hw.pred <- predict(best.hw.mod, n.ahead=N_TEST)

plot.mod(best.hw.mod, best.hw.pred, 'Best HW')
(best.hw.rmse <- get.rmse(best.hw.pred, test))
