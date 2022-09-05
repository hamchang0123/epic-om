pacman::p_load(tidyverse, rio, deSolve, reshape2, lubridate, ggpubr)

#---Import OWID dataset for Korea
raw <- import('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/vax_cov_overall.csv')

test <- raw %>%
  filter(iso_code == 'KOR') %>% 
  select(date, contains('test'), contains('case')) %>% 
  mutate(date = ymd(date))

test <- test %>% 
  filter(date > '2021-10-31' & date < '2022-07-01')

ggplot()+
  geom_col(data = test, aes(x=date, y=new_tests_per_thousand))+
  scale_x_date(date_breaks = '2 months')+
  geom_vline(xintercept = as.Date('2022-02-03'), linetype=2, colour='light grey')+
  labs(x = 'Date', y = 'New tests per thousand people')+
  theme_classic2(base_size = 10)
  
ggplot()+
  #geom_col(data = test, aes(x=date, y=new_tests_per_thousand))+
  #geom_line(data = test, aes(x=date, y=tests_per_case))+
  geom_col(data = test, aes(x = date, y = new_cases_per_million), colour = 'red')+
  scale_x_date(date_breaks = '2 months')+
  geom_vline(xintercept = as.Date('2022-02-03'), linetype=2, colour='light grey')+
  labs(x = 'Date', y = 'Tests per case')+
  annotate(geom = 'text', x = as.Date('2021-12-15'), y = 70, label = 'Open public testing', size = 3)+
  annotate(geom = 'text', x = as.Date('2022-04-11'), y = 70, label = 'Restricted PCR testing', size = 3)+
  theme_classic2(base_size = 10)
