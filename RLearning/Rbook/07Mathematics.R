#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/R/RLearning/Rbook')



# 1. Mathematical Functions


# 1.1 Exponential Functions
# log, exp


# 1.2. Trig Functions
# sin, cos, tan, ...


# 1.3 Power Laws
# x^y


# 1.4 Polynomial Functions


