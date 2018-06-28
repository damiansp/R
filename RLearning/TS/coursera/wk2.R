#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/RLearning/TS/coursera')

library(astsa)

# 1. Visualizing and Describing TS: Time Plots, Autocovariance & Autocorrelation
# 1.1 TIme plots
class(jj)
plot(jj, 
     type='o', 
     main='Johnson & Johnson quarterly earnings per share', 
     xlab='Year', 
     ylab='Earnings')
class(flu)
plot(flu, 
     main='Monthly Pneumonia and Flu Deaths (US)', 
     xlab='Months', 
     ylab='Deaths per 10K')
class(globtemp)
plot(globtemp, 
     main='Global mean land-ocean deviations from mean temp 1951-80', 
     xlab='Year',
     ylab='Temperature Deviation')
class(globtempl)
plot(globtempl,
     main='Global mean land deviations from mean temp 1951-80', 
     xlab='Year',
     ylab='Temperature Deviation')
class(star)
plot(star,
     xlab='Days',
     ylab='Magnitude',
     main='Magnitude of star at midnight form 600 consecutive days')
     
# 1.2 First Intuitions on (Weak) Stationarity
# - no trend, constant variance, no periodicity
