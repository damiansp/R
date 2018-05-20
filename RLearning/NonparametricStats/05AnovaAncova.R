# Chapter 5 ANOVA and ANCOVA

rm(list=ls())
setwd('~/Learning/R/RLearning/NonparametricStats')

library(npsm)
library(Rfit)
data(plank)
data(quail)
data(serumLH)

# Make some artificial data
A <- rnorm(30, 0, 1)
B <- rnorm(30, 2, 0.5)
C <- rnorm(30, 4, 3)
D <- rnorm(30, 1, 0.25)
ldl <- c(A, B, C, D)
treatment <- rep(c(1, 2, 3, 4), each=30)
fake <- data.frame(ldl=ldl, treatment=treatment)
head(fake)



# 2. One-Way ANOVA
robust.quail.fit <- oneway.rfit(quail$ldl, quail$treat)
robust.quail.fit
boxplot(ldl ~ treat, data=quail)

robust.fake.fit  <- oneway.rfit(fake$ldl, fake$treatment)
robust.fake.fit
boxplot(ldl ~ treatment, data=fake)

# 2.1 Multiple Comparisons
summary(robust.quail.fit, method='tukey')
summary(robust.fake.fit, method='tukey')

# 2.2 Kruskal-Wallis Test
# times to clear half dust build up in cilia 
normal <- c(2.9, 3.0, 2.5, 2.6, 3.2)
obstructed <- c(3.8, 2.7, 4.0, 2.4)
asbestosis <- c(2.8, 3.4, 3.7, 2.2, 2.0)
x <- c(normal, obstructed, asbestosis)
g <- c(rep(1, 5), rep(2, 4), rep(3, 5))
cilia <- data.frame(x=x, g=g)
boxplot(x ~ g, data=cilia)
kruskal.test(x, g) # p = 0.68, no evidence of difference by group



# 3. Multi-Way Crossed Factorial Design
# 3.1 Two-Way
head(serumLH)
# robust anova
raov(serum ~ light.regime * LRF.dose, data=serumLH)

# Compare to LS ANOVA:
summary(aov(serum ~ light.regime * LRF.dose, data=serumLH))

# 3.2 k-Way
head(plank)
raov(response ~ strain * gender * age, data=plank)
anova(lm(response ~ strain * gender * age, data=plank))