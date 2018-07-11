#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')

library(asaur)
library(muhaz)    # est./plot nonparam hazard funcs
library(survival)



# 1. Comparing Two Groups of Survival Times
tt <- c(6, 7, 10, 15, 19, 25)
delta <- c(1, 0, 1, 1, 0, 1) # Censored
trt <- c(0, 0, 1, 0, 1, 1)   # Group assignment; treatment
survdiff(Surv(tt, delta) ~ trt)

head(pancreatic)
date.format <- '%m/%d/%Y'
pancreatic$progression <- as.Date(as.character(pancreatic$progression), 
                                  format=date.format)
pancreatic$onstudy <- as.Date(as.character(pancreatic$onstudy), 
                              format=date.format)
pancreatic$death <- as.Date(as.character(pancreatic$death), format=date.format)
progression.only <- pancreatic$progression - pancreatic$onstudy
overall.survival <- pancreatic$death - pancreatic$onstudy
pfs <- progression.only
pfs[is.na(pfs)] <- overall.survival[is.na(pfs)]
pfs.month <- pfs / 30.417

plot(survfit(Surv(pfs.month) ~ pancreatic$stage), 
     xlab='Months', 
     ylab='P(Surv)', 
     col=c(4, 2))
legend(
  'topright', lty=1, col=c(4, 2), legend=c('Locally Advanced', 'Metastatic'))
survdiff(Surv(pfs) ~ pancreatic$stage, rho=1)



# 2 Stratified Test
head(pharmacoSmoking)
survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp)

lambda.mutant.0 <- 0.03 # hazard
lambda.mutant.1 <- 0.03 * 0.55
lambda.wt.0 <- 0.03 * 0.2
lambda.wt.1 <- 0.03 * 0.2 * 0.55
set.seed(1103)
tt.control.mutant <- rexp(25, rate=lambda.mutant.0)
tt.treat.mutant <- rexp(125, lambda.mutant.1)
tt.control.wt <- rexp(125, lambda.wt.0)
tt.treat.wt <- rexp(25, lambda.wt.1)
tt.all <- c(tt.control.mutant, tt.treat.mutant, tt.control.wt, tt.treat.wt)
status <- rep(1, length(tt.all))
genotype <- c(rep('mutant', 150), rep('wt', 150))
trt <- c(rep(0, 25), rep(1, 125), rep(0, 125), rep(1, 25))
survdiff(Surv(tt.all, status) ~ trt)
survdiff(Surv(tt.all, status) ~ trt + strata(genotype))
