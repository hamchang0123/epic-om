#--Set up
setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om')
source('./main/parameters.R')
pacman::p_load(magrittr, rio, tidyverse)

#--Import observed incidence in adults
obs_a <- rio::import(file = './data/obs_a.csv')

obs_a <- obs_a %>% 
  filter(dates > '2022-01-15' & dates < '2022-07-01')%>%
  mutate(dates = as.Date(dates), time = 0:165)

#--Import observed incidence in children
obs_y <- rio::import(file = './data/obs_y.csv')

obs_y <- obs_y %>% 
  filter(dates > '2022-01-15' & dates < '2022-07-01')%>%
  mutate(dates = as.Date(dates), time = 0:165)

#--Create total observed incidence
obs_t <- data.frame(
  time = 0:165,
  daily = obs_a$daily + obs_y$daily
)

obs_t %>% 
  summarise(cumulative = sum(Dt_per_m))

obs_t %>% 
  filter(Dt_per_m == max(Dt_per_m))
