rm(list = ls())
setwd('~/Learning/R/RLearning/GAM')
library(gamair)
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