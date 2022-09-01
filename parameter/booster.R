pacman::p_load(tidyverse, rio, deSolve, reshape2, lubridate, ggpubr, scales)

#---Import OWID dataset for Korea
boo_raw <- import('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/booster_cov.csv')

boo_kor <- boo_raw %>% 
  filter(Code == 'KOR') %>% 
  mutate(Day = mdy(Day))

boo_kor %>% 
  filter(total_boosters_per_hundred > 0) %>% 
  summarise(start = min(Day))
  #Booster vaccination started from 2021-09-27

#---Plot
boo_kor <- boo_kor %>% 
  filter(total_boosters_per_hundred > 0)

ggplot() +
  geom_line(data = boo_kor, aes(x = Day, y = total_boosters_per_hundred))+
  theme_classic2(base_size=10)+
  labs(x = 'Time (Month)', y = '% population that received booster')
