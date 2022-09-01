source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters.R')
pacman::p_load(magrittr, rio, tidyverse)

obs_a <- rio::import(file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/obs_a.csv')


obs_a <- obs_a %>% 
  filter(dates > '2022-01-15' & dates < '2022-07-01')%>%
  mutate(dates = as.Date(dates), time = 0:165)

obs_y <- rio::import(file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/obs_y.csv')

obs_y <- obs_y %>% 
  filter(dates > '2022-01-15' & dates < '2022-07-01')%>%
  mutate(dates = as.Date(dates), time = 0:165)


obs_t <- data.frame(
  time = 0:165,
  daily = obs_a$daily + obs_y$daily
)

obs_t <- obs_t %>%
  mutate(Dt_per_m = daily / parameters['popn'] *1E6)

obs_a <- obs_a %>%
  mutate(Da_per_m = daily / parameters['popn'] *1E6)

obs_y <- obs_y %>%
  mutate(Dy_per_m = daily / parameters['popn'] *1E6)


obs_t %>% 
  summarise(cumulative = sum(Dt_per_m))

obs_t %>% 
  filter(Dt_per_m == max(Dt_per_m))
