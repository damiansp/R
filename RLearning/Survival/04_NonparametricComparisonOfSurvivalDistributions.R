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