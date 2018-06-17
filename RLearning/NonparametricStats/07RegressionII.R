# Chapter 7 Regression II
# https://github.com/kloke/hbrfit
# https://github.com/kloke/npsmReg2

rm(list=ls())
setwd('~/Learning/R/RLearning/NonparametricStats')

library(MASS)
library(quantreg)
library(Rfit)
library(robustbase)
load('data/stars.rda')


diagplot <- function (fit, ...) {
  par(mfrow=c(2, 2))
  plot(fitted.values(fit), 
       residuals(fit), 
       xlab="Fit", 
       ylab="Residual", 
       main="Residuals vs. Fits")
  hist(residuals(fit), 
       freq=F,
       main="Histogram of Residuals", 
       xlab="Residual")
  plot(rstudent(fit), 
       xlab="Case", 
       ylab="Studentized Residual", 
       main="Case Plot of\nStudentized Residuals")
  abline(h=c(-2, 2))
  qqnorm(residuals(fit), main="Normal Q-Q Plot of Residuals")
  qqline(residuals(fit))
}


psi <- function (x) {
  x[x == -Inf] <- -100
  x[x == Inf] <- 100
  -1*(x <= -1) + x*(-1 < x & x < 1) + 1*(x >= 1)
}

robdist.hbrfit <- function (x) {
  x.o <- x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  qn <- floor((n + p + 1)/2)
  if (qn < p + 1) 
    stop(paste("quantile must be at least", p + 1))
  divisor <- apply(x, 2, IQR)
  if (any(divisor == 0)) 
    stop("mycov.rob:  at least one column has IQR 0")
  x <- x/rep(divisor, rep(n, p))
  if (p > 1) {
    best <- covMcd(x)$best
    if (!length(best)) {
      stop("x is probably collinear")
    } else {
      means <- colMeans(x[best, , drop=F])
      rcov <- var(x[best, , drop=F]) * (1 + 15/(n - p))^2
    }
  } else {
    z <- covMcd(x)
    means <- z$center
    rcov <- z$cov
  }
  dist <- mahalanobis(x, means, rcov)
  cut <- qchisq(0.975, p) * quantile(dist, qn/n)/qchisq(qn/n, p)
  center = colMeans(x[dist < cut, , drop=F]) * divisor
  cov <- divisor * var(x[dist < cut, , drop=F]) * rep(divisor, rep(p, p))
  mahalanobis(x.o, center, cov)
}


hbrwts <- function(x, y, percent=0.95, ehat0=ltsreg(x, y)$residuals) {
  x <- as.matrix(x)
  n <- dim(x)[1]
  p <- dim(x)[2]
  robdis2 <- robdist.hbrfit(x)
  cut <- qchisq(percent, p)
  sigma <- mad(ehat0)
  m <- psi(cut/robdis2)
  a <- ehat0/(sigma * m)
  c <- (median(a) + 3*mad(a))^2
  h <- sqrt(c) / a
  tmp <- pairup(h)
  psi(abs(tmp[, 1] * tmp[, 2]))
}


hbrfit <- function (formula, data, subset, symmetric = FALSE,...) {
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf <- eval.parent(mf)
  x.o <- x <- model.matrix(attr(mf, "terms"), data=mf)
  y.o <- y <- model.response(mf)
  x <- as.matrix(x[, colnames(x) != "(Intercept)"])
  x1 <- cbind(rep(1, nrow(x)), x)
  qrx1 <- qr(x1)
  Q <- as.matrix(qr.Q(qrx1))
  q1 <- Q[, 1]
  x <- as.matrix(Q[, 2:qrx1$rank])
  n <- nrow(x)
  p <- ncol(x)
  bij <- hbrwts(x, y)
  ypairs <- pairup(y)
  yi <- ypairs[, 1]
  yj <- ypairs[, 2]
  xpairs <- pairup(x)
  xi <- xpairs[, 1:p]
  xj <- xpairs[, (p + 1):(2 * p)]
  ystar <- bij * (yi - yj)
  xstar <- bij * (xi - xj)
  est <- rq(
    ystar ~ xstar - 1, 
    method=ifelse(length(ystar) > 5000 | p > 20, "fnb", "br"))$coefficients
  int <- median(y - (x %*% as.matrix(est)))
  resid <- as.vector(y - int - (x %*% as.matrix(est)))
  yhat <- y - resid
  bhat <- lsfit(x.o, yhat, intercept=F)$coefficients
  wts <- matrix(0, nrow=n, ncol=n)
  index <- pairup(1:n)
  wts[index] <- bij
  wts[index[, 2:1]] <- bij
  tauhat <- gettauF0(resid, p)
  if (symmetric) {
    taushat <- tauhat
  } else {
    taushat <- taustar(resid, p)
  }
  ans <- list(
    coefficients=bhat, residuals=resid, fitted.values=y - resid, weights=wts, 
    y=y.o, x=x.o, tauhat=tauhat, taushat=taushat, betahat=bhat, qrx1=qrx1)
  ans$call <- call
  class(ans) <- c("hbrfit", "rfit")
  ans
}

# 2. High Breakdown Rank-Based Fits
plot(stars$light ~ stars$temp)
fit.hbr <- hbrfit(stars$light ~ stars$temp)
diagplot(fit.hbr)
