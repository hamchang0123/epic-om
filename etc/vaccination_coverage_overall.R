pacman::p_load(tidyverse, rio, deSolve, reshape2, lubridate, ggpubr, scales)

#---Import OWID dataset for Korea
vax_raw <- import('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/vax_cov_overall.csv')

vax_kor <- vax_raw %>% 
  filter(iso_code == 'KOR') %>% 
  mutate(date = ymd(date))

vax_kor %>% 
  filter(people_vaccinated > 0) %>% 
  summarise(date = min(date))

vax_kor <- vax_kor %>% 
  filter(date < '2022-07-01' & date > '2021-02-25') %>% 
  select(date, contains('vacc'))


ggplot(vax_kor, aes(x= date)) + 
  geom_line(aes(y = people_fully_vaccinated_per_hundred))+
  labs(x = 'Date', y = 'Double vaccinated people in the population (%)')+
  scale_x_date(date_breaks = "2 months")+
  geom_hline(yintercept = 50, colour = 'light grey', alpha = 0.5)+
  geom_hline(yintercept = 75, colour = "light grey", alpha = 0.5)+
  geom_vline(xintercept = as.Date('2021-09-30'), colour = 'light grey', linetype=3)+
  geom_vline(xintercept = as.Date('2021-10-30'), colour = 'light grey', linetype=3)+
  theme_classic2(base_size = 10)

vax_kor %>% 
  filter(date == '2021-08-09')
  #ppl fully vaccinated per 100 = 15.61

vax_kor %>% 
  filter(people_fully_vaccinated_per_hundred == 50)
  #date = 2021-09-30

vax_kor %>% 
  filter(people_fully_vaccinated_per_hundred > 74.9) %>% 
  summarise(date = min(date))
  #date = 2021-10-30

vax_kor %>% 
  filter(date == '2021-11-01')
