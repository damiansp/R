#=========#=========#=========#=========#=========#=========#=========#=========
rm(list = ls())
setwd('~/Learning/R/RLearning/GAM')
library(gamair)
library(nlme)
data(Rail)
data(stomata)



# 1. Mixed Models for Balanced Data
# 1.1 A motivating example (wrong approach!)
m1 <- lm(area ~ CO2 + tree, stomata)
m0 <- lm(area ~ CO2, stomata)
anova(m0, m1)
# strong evidence for tree-to-tree variation, obscures effect of CO2

m2 <- lm(area ~ tree, stomata)
anova(m1, m2) # suggests tree only is the better model!

# (The right approach)
st <- aggregate(data.matrix(stomata), by=list(tree=stomata$tree), mean)
st$CO2 <- as.factor(st$CO2)
st
stomata
m3 <- lm(area ~ CO2, st)
anova(m3)

# between-tree variance
summary(m3)$sigma^2 - summary(m1)$sigma^2 / 4

# 1.3 A single random factor
head(Rail)
m1 <- lm(travel ~ Rail, Rail)
anova(m1)

rt <- aggregate(data.matrix(Rail), by=list(Rail$Rail), mean)
rt
m0 <- lm(travel ~ 1, rt)
sig <- summary(m1)$sigma
sigb <- (summary(m0)$sigma^2 - sig^2 / 3)^0.5
sigb
sig
summary(m0)