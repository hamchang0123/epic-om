pacman::p_load(tidyverse, rio, deSolve, reshape2)

#---Import Google Mobility dataset
ggl_mob_raw <- import('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/google_mobility.csv')

#---Mobility at public transportation
mob_transit <- ggl_mob_raw %>%
  filter(Code=='KOR') %>%
  mutate(Date = lubridate::mdy(Day)) %>%
  filter(Date > '2022-01-15' & Date < '2022-07-01') %>%
  dplyr::select(Date, transit_stations)

mob_transit <- mob_transit %>%
  mutate(time = seq(0:165)-1)

ggplot()+
  geom_line(data=mob_transit, aes(x=time, y=transit_stations))+
  scale_x_continuous(breaks = c(0, 13, 17, 22, 44, 92, 165))+
  theme_classic2(base_size=12)+
  labs(x = 'Time (day)', y = 'Mobility at transit stations')

#---Mobility at residential places
mob_res <- ggl_mob_raw %>%
  filter(Code=='KOR') %>%
  mutate(Date = lubridate::mdy(Day)) %>%
  filter(Date > '2022-01-15' & Date < '2022-07-01') %>%
  dplyr::select(Date, residential)

mob_res <- mob_res %>%
  mutate(time = seq(0:165)-1)

ggplot()+
  geom_line(data=mob_res, aes(x=time, y=residential))+
  scale_x_continuous(breaks = c(0, 13, 17, 22, 44, 92, 165))+
  theme_classic2(base_size=12)+
  labs(x = 'Time (day)', y = 'Mobility at residential areas')

mob_res %>%
  filter(Date > '2022-01-28' & Date < '2022-02-07') %>% 
  summarise(mean = mean(residential))

mob_transit %>%
  filter(Date > '2022-01-28' & Date < '2022-02-07') %>% 
  summarise(mean = mean(transit_stations))

mob_rr %>%
  filter(Date > '2022-01-28' & Date < '2022-02-07') %>% 
  summarise(mean = mean(retail_and_recreation))

#---Mobility at retail & recreation
mob_rr <- ggl_mob_raw %>%
  filter(Code=='KOR') %>%
  mutate(Date = lubridate::mdy(Day)) %>%
  filter(Date > '2022-01-15' & Date < '2022-07-01') %>%
  dplyr::select(Date, retail_and_recreation)%>%
  mutate(time = seq(0:165)-1)

ggplot()+
  geom_line(data=mob_rr, aes(x=time, y=retail_and_recreation))+
  scale_x_continuous(breaks = c(0, 13, 17, 22, 44, 92, 165))+
  theme_classic2(base_size=12)+
  labs(x = 'Time (day)', y = 'Mobility at retail and recreational venues')
