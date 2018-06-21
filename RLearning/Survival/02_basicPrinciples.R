#-------#-------#-------#-------#-------#-------#-------#-------#-------#-------
rm(list=ls())
setwd('~/Learning/R/RLearning/Survival')


library(survival)

# 1. Hazard and Survival Functions
head(survexp.us)
str(survexp.us)
dimnames(survexp.us)
tm <- c(0,      # birth
        1/365,  # 1 day
        7/365,  # 1 week
        28/365, # 4 weeks
        1:109)  # subsequent years        
haz.male <- survexp.us[, 'male', '2004']     # males 2004 cohort
haz.female <- survexp.us[, 'female', '2004'] # females 2004 cohort

plot(haz.male ~ tm, 
     type='l', 
     log='y', 
     xlab='Age (years)', 
     ylab='log(Hazard)',
     main='Expected Death Hazard for Americans Born in 2004')
lines(haz.female ~ tm, col=2)
legend('bottomright', lty=1, col=1:2, legend=c('male', 'female'), bty='n')



# 2 Other Representations of a Survival Distribution
