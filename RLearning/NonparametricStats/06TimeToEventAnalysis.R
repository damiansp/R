#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/NonparametricStats')

library(npsm)
library(Rfit)
library(survival)
data(cancertrt)
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


# 2.1 Gehan's test
head(cancertrt)
args(gehan.test)
gehan.test(cancertrt$time, cancertrt$event, cancertrt$trt)



# 3. Cox Proportional Hazard Models
fit <- coxph(Surv(time, recur) ~ genotype, data=hemorrhage)
summary(fit3) # The coef, 1.33 indicates increased risk for genotype of group 2
# Alternately est. risk of hemorrhage for hetero is 3.878 over homo 

head(prostate)
f2 <- coxph(Surv(time, event=status) ~ as.factor(treatment) + size + index, 
            data=prostate)
summary(f2)



# 4. Accelerated Failure Time Models
