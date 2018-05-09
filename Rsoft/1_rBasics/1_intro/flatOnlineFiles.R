library(dplyr)
library(readr)

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
ext.tracks[1:5, 1:9]

ext.tracks %>%
  filter(storm_name == 'KATRINA') %>%
  select(month, day, hour, max_wind, min_pressure, rad_max_wind) %>%
  sample_n(4)
  
zika.file <- paste0(
  'https://raw.githubusercontent.com/cdcepi/zika/master/Brazil/',
  'COES_Microcephaly/data/COES_Microcephaly-2016-06-25.csv')
zika.brazil <- read_csv(zika.file)
zika.brazil %>% select(location, value, unit)