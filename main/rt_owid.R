setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om')
source('./main/parameters.R')
source('./main/r_baseline.R')
pacman::p_load(tidyverse, rio, deSolve, reshape2, lubridate, ggpubr)

#---Import OWID dataset for Korea
raw <- import('./data/vax_cov_overall.csv')

r <- raw %>%
  filter(iso_code == 'KOR') %>% 
  select(date, reproduction_rate) %>% 
  mutate(date = ymd(date))

r <- r %>% 
  filter(date > '2022-01-15' & date < '2022-07-01')

r <- r %>% 
  mutate(time = 0:(nrow(r)-1))

r %>% 
  filter(time < 22) %>% 
  summarise(mean = mean(reproduction_rate))
  #1.598

r %>% 
  filter(time > 21 & time < 44) %>% 
  summarise(mean = mean(reproduction_rate))
  #1.575

r %>% 
  filter(time > 43 & time < 92) %>% 
  summarise(mean = mean(reproduction_rate))
  #1.032

r %>% 
  filter(time > 91 & time < 166) %>% 
  summarise(mean = mean(reproduction_rate))
  #0.730