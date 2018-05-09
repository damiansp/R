library(dplyr)
library(faraway)
library(ggplot2)
library(knitr)
library(readr)
library(tidyr)
data(worldcup)
data(VADeaths)


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

  
knots.to.mph <- function(knots) {
  mph <- 1.152 * knots
}

ext.tracks %>%
  summarize(n.obs=n(), 
            worst.wind=knots.to.mph(max(max_wind)), 
            worst.pressure=min(min_pressure))
ext.tracks %>%
  group_by(storm_name, year) %>%
  summarize(n.obs=n(), 
            worst.wind=knots.to.mph(max(max_wind)), 
            worst.pressure=min(min_pressure))
ext.tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = knots.to.mph(max(max_wind))) %>%
  ggplot(aes(x=worst_wind)) + geom_histogram()
  


# Selecting and Filtering Data
ext.tracks %>% 
  select(storm_name, month, day, hour, year, latitude, longitude, max_wind)
ext.tracks %>% 
  select(storm_name, latitude, longitude, starts_with('radius_34'))
  
ext.tracks %>%
  select(storm_name, hour, max_wind) %>%
  head(9)
ext.tracks %>%
  select(storm_name, hour, max_wind) %>%
  filter(hour == '00') %>%
  head(5)
ext.tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind = max(max_wind)) %>%
  filter(worst_wind >= 140)
ext.tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name == 'ANDREW' & max_wind >= 130)



# Adding and Changing Columns
worldcup <- worldcup %>% mutate(PlayerName=rownames(worldcup))
worldcup <- worldcup %>%
  group_by(Position) %>%
  mutate(MeanShots=mean(Shots)) %>%
  ungroup()
worldcup <- worldcup %>% rename(Name=PlayerName)  
head(worldcup)



# Spreading and Gathering Data
head(VADeaths)
VADeaths <- VADeaths %>%
  tbl_df() %>%
  mutate(age=row.names(VADeaths))
VADeaths
VADeaths %>% gather(key=key, value=death.rate, -age)

worldcup %>%
  select(Position, Time, Shots, Tackles, Saves) %>%
  gather(Type, Number, -Position, -Time) %>%
  ggplot(aes(x=Time, y=Number)) + geom_point() + facet_grid(Type ~ Position)
  
worldcup.t <- worldcup %>%
  filter(Team %in% c('Spain', 'Netherlands', 'Uruguay', 'Germany')) %>%
  select(Team, Position, Passes) %>%
  group_by(Team, Position) %>%
  summarize(
    MeanPasses=mean(Passes),
    MinPasses=min(Passes),
    MaxPasses=max(Passes),
    PassSummary=paste0(
      round(MeanPasses), ' (', MinPasses, ', ', MaxPasses, ')')) %>%
  select(Team, Position, PassSummary)
worldcup.t  

worldcup.t %>%
  spread(Position, PassSummary) %>%
  kable()
  
  
  
# Merging Datasets
worldcup %>%
  mutate(Name=rownames(worldcup), Team=as.character(Team)) %>%
  select(Name, Position, Shots, Team) %>%
  arrange(desc(Shots)) %>% # sort descending
  left_join(team.standings, by='Team') %>%
  rename('TeamStanding'=Standing)