#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/R/Rsoft/4_largeDataSets')

library(dplyr)
#library(readr)

spec <- read.csv('../data/daily_SPEC_2014.csv.bz2')
spec$site <- paste(spec$State.Code, spec$County.Code, spec$Site.Num, sep='_')
spec$date <- as.Date(spec$Date.Local)
head(spec)
names(spec)

# 1
mean(spec[(spec$State.Name == 'Wisconsin' 
           & spec$Parameter.Name == 'Bromine PM2.5 LC'), 
          c('Arithmetic.Mean')], 
     na.rm=T) 
     
# 2
sort(tapply(spec$Arithmetic.Mean, spec$Parameter.Name, mean, na.rm=T))

# 3
sulfate <- subset(spec, Parameter.Name == 'Sulfate PM2.5 LC')
sulfate$site <- paste(
  sulfate$State.Code, sulfate$County.Code, sulfate$Site.Num, sep='_')
head(sulfate)
sort(tapply(sulfate$Arithmetic.Mean, sulfate$site, function(x) mean(x, na.rm=T)))

# 4
ecpm <- subset(spec, Parameter.Name == 'EC PM2.5 LC TOR' 
               & State.Name %in% c('California', 'Arizona'))
ecpm.by.state <- tapply(ecpm$Arithmetic.Mean, ecpm$State.Name, mean, na.rm=T)
abs(ecpm.by.state['California'] - ecpm.by.state['Arizona'])

# 5
western <- subset(spec, Longitude < -100)
median(western[western$Parameter.Name == 'OC PM2.5 LC TOR', 'Arithmetic.Mean'])

# 6
aqs <- read.csv('../data/aqs_sites.csv')
res.sub <- subset(aqs, Land.Use == 'RESIDENTIAL' & Location.Setting == 'SUBURBAN')
dim(res.sub)


# 7
res.sub.sites <- paste(
  res.sub$State.Code, res.sub$County.Code,res.sub$Site.Num, sep='_')
eastern <- subset(spec, Longitude >= -100)
eastern.res.sub <- subset(eastern, site %in% res.sub.sites)
median(eastern.res.sub[eastern.res.sub$Parameter.Name == 'EC PM2.5 LC TOR', 
                       'Arithmetic.Mean'])
                       
# 8
aqs$site <- paste(aqs$State.Code, aqs$County.Code, aqs$Site.Num, sep='_')
com.sites <- subset(aqs, Land.Use == 'COMMERCIAL')$site
com.sulfate <- subset(sulfate, site %in% com.sites)
com.sulfate$month <- format(as.Date(com.sulfate$date), '%m')

names(com.sulfate)
sort(tapply(com.sulfate$Arithmetic.Mean, com.sulfate$month, mean, na.rm=T))
barplot(tapply(com.sulfate$Arithmetic.Mean, com.sulfate$month, mean, na.rm=T))

# 9
s <- subset(spec, site == '6_65_8001')
s.sulfate <- s[s$Parameter.Name == 'Sulfate PM2.5 LC', 
               c('date', 'Arithmetic.Mean')]
s.nitrate <- s[s$Parameter.Name == 'Total Nitrate PM2.5 LC', 
               c('date', 'Arithmetic.Mean')]
s.sulfate$date == s.nitrate$date
sum(s.sulfate$Arithmetic.Mean + s.nitrate$Arithmetic.Mean > 10)

# 10
sulfate <- subset(spec, Parameter.Name == 'Sulfate PM2.5 LC')
nitrate <- subset(spec, Parameter.Name == 'Total Nitrate PM2.5 LC')
sulf <- sulfate[, c('date', 'Arithmetic.Mean', 'site')]
nitr <- nitrate[, c('date', 'Arithmetic.Mean', 'site')]
sulf.nitr <- left_join(sulf, nitr, by=c('site', 'date'))
head(sulf.nitr)
tapply(sulf.nitr$)

highest <- 0.0
h.site <- ''

for (s in unique(sulf.nitr$site)) {
  sub <- subset(sulf.nitr, site == s)
  c <- abs(cor(
    sub$Arithmetic.Mean.x, sub$Arithmetic.Mean.y, use='pairwise.complete.obs'))
  if (!is.na(c) 
      & (c > highest 
         | s %in% c('42_45_2', '5_113_3', '16_37_2', '2_90_35'))) {
    highest <- c
    h.site <- s
    cat(sprintf('Cor: %.3f (%s)\n', c, s))
  }
}
