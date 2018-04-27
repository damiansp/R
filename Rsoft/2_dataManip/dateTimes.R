library(dplyr)
library(gridExtra)
library(lubridate)
library(tidyr)
rm(list=ls())

ext.track.file <- paste0('http://rammb.cira.colostate.edu/research/',
                         'tropical_cyclones/tc_extended_best_track_dataset/',
                         'data/ebtrk_atlc_1988_2015.txt')
# Column widths
ext.track.widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                      4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext.track.col.names <- c(
  'storm_id', 'storm_name', 'month', 'day', 'hour', 'year', 'latitude', 
  'longitude', 'max_wind', 'min_pressure', 'rad_max_wind', 'eye_diameter',
  'pressure_1', 'pressure_2',
  paste('radius_34', c('ne', 'se', 'sw', 'nw'), sep='_'),
  paste('radius_50', c('ne', 'se', 'sw', 'nw'), sep='_'),
  paste('radius_60', c('ne', 'se', 'sw', 'nw'), sep='_'),
  'storm_type', 'distance_to_land', 'final')
ext.tracks <- read_fwf(
  ext.track.file, fwf_widths(ext.track.widths, ext.track.col.names), na='-99')
  
  
# Lubridate basics
ymd('1976-11-03')
ymd('76 Nov 03')
ymd_hm('76/11/3 1:23 am')


andrew.tracks <- ext.tracks %>%
  filter(storm_name == 'ANDREW' & year == '1992') %>%
  select(year, month, day, hour, max_wind, min_pressure) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime=ymd_h(datetime))
  
head(andrew.tracks)
class(andrew.tracks$datetime)

andrew.tracks %>%
  gather(measure, value, -datetime) %>%
  ggplot(aes(x=datetime, y=value)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~ measure, ncol=1, scales='free_y')
  
  
  
# Pulling out Date/Time Elements
andrew.tracks %>%
  select(datetime) %>%
  mutate(year=year(datetime), 
         month=months(datetime), 
         weekday=weekdays(datetime), 
         yday=yday(datetime), 
         hour=hour(datetime))
         
check.tracks <- ext.tracks %>%
  select(month, day, hour, year, max_wind) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(
    datetime=ymd_h(datetime),
    weekday=weekdays(datetime),
    weekday=factor(
      weekday, 
      levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
               'Saturday')),
    month=months(datetime),
    month=factor(
      month, 
      levels=c('April', 'May', 'June', 'July', 'August', 'September', 'October',
               'November', 'December', 'January', 'February', 'March')))

check.weekdays <- check.tracks %>%
  group_by(weekday) %>%
  summarize(max_max_wind=max(max_wind)) %>%
  rename(grouping=weekday)
check.months <- check.tracks %>%
  group_by(month)  %>%
  summarize(max_max_wind=max(max_wind)) %>%
  rename(grouping=month)
  
a <- ggplot(check.weekdays, aes(x=grouping, y=max_max_wind)) +
  geom_bar(stat='identity') + xlab('')
b <- a %+% check.months
grid.arrange(a, b, ncol=1)