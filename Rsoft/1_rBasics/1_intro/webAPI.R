library(httr)

meso.url <- 'https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py/'
denver <- GET(url=meso.url,
              query=list(station='DEN', data='sped', year1='2016', month1='6',
                         day1='1', year2='2016', month2='6', day2='30', 
                         tz='America/Denver', format='comma')) %>%
  content() %>%
  read_csv(skip=5, na='M')
denver %>% slice(1:3)
