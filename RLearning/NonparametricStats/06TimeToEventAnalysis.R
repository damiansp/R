#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/NonparametricStats')

library(npsm)
library(Rfit)
library(survival)
data(hemorrhage)

# 2 Kaplan-Meier and Log Rank Test
head(hemorrhage)
Surv(round(hemorrhage$time, 2), hemorrhage$recur)

fit <- survfit(Surv(hemorrhage$time, hemorrhage$recur) ~ hemorrhage$genotype)
fit
summary(fit)
plot(fit, 
     col=1:2, 
     ylab='Probablity of hemorrhage-free survival', 
     xlab='Time (months)')
legend('bottomleft', legend=c('Group 1', 'Group 2'), lty=1, col=1:2, bty='n')

survdiff(Surv(hemorrhage$time, hemorrhage$recur) ~ hemorrhage$genotype)
# On avg. genotype = 0 expected to live 9.28 - 8.72 (= 0.56) months longer 
# (p = 0.01)