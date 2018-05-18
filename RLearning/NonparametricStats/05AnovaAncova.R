# Chapter 5 ANOVA and ANCOVA

rm(list=ls())
setwd('~/Learning/R/RLearning/NonparametricStats')

library(Rfit)
data(quail)

# Make some artificial data
A <- rnorm(30, 0, 1)
B <- rnorm(30, 2, 0.5)
C <- rnorm(30, 4, 3)
D <- rnorm(30, 1, 0.25)
ldl <- c(A, B, C, D)
treatment <- rep(c(1, 2, 3, 4), each=30)
fake <- data.frame(ldl=ldl, treatment=treatment)
head(fake)

# 2 One-Way ANOVA
robust.quail.fit <- oneway.rfit(quail$ldl, quail$treat)
robust.quail.fit
boxplot(ldl ~ treat, data=quail)

robust.fake.fit  <- oneway.rfit(fake$ldl, fake$treatment)
robust.fake.fit
boxplot(ldl ~ treatment, data=fake)

# 2.1 Multiple Comparisons
summary(robust.quail.fit, method='tukey')
summary(robust.fake.fit, method='tukey')