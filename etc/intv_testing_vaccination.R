pacman::p_load(tidyverse, rio, deSolve, reshape2, lubridate, ggpubr)

#---Import OxCGRT dataset for Korea
intv_raw <- import('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/OxCGRT_latest.csv')

intv_kor <- intv_raw %>% 
  filter(CountryCode == 'KOR') %>% 
  mutate(Date = ymd(Date)) %>%
  filter(Date > as.Date('2022-01-15') & Date < as.Date('2022-07-01'))

#---Filter H-related measures
intv_kor_h <- intv_kor %>%
  dplyr::select(Date, 
                `H1_Public information campaigns`, 
                `H2_Testing policy`, 
                `H3_Contact tracing`, 
                `H4_Emergency investment in healthcare`) %>%
  rename('info' = `H1_Public information campaigns`, 
         'testing' = `H2_Testing policy`, 
         'ct' = `H3_Contact tracing`, 
         'invest' = `H4_Emergency investment in healthcare`)

intv_kor_h <- intv_kor_h %>%
  mutate(time = 0:(nrow(intv_kor_h)-1)) %>%
  select(time, info, testing, ct)

ggplot(data= intv_kor_h, aes(x=time)) + 
  geom_line(aes(y=info), linetype=2)+
  geom_line(aes(y=testing), linetype=1, colour = 'light grey')+
  geom_line(aes(y=ct), linetype=3, colour = 'red')


#---Filter V-related measures
intv_kor_v <- intv_raw %>% 
  filter(CountryCode == 'KOR') %>% 
  mutate(Date = ymd(Date)) %>%
  select(Date, starts_with('V2A')) %>% 
  rename('va' = 'V2A_Vaccine Availability (summary)')%>%
  filter(Date < '2022-07-01')

ggplot(data= intv_kor_v, aes(x=Date)) + 
  geom_line(aes(y=va), linetype=1) +
  scale_x_date(breaks = c(as.Date('2020-01-01'), 
                          as.Date('2022-06-30'), 
                          as.Date('2021-10-05'), 
                          as.Date('2021-08-09'), 
                          as.Date('2021-02-26')),
               labels = c('20/01/01', '22/06/30', '21/10/05', '21/08/09', '21/02/26'))+
  labs(y = 'Vaccine Availability')+
  theme_classic2()

intv_kor_v %>% 
  filter(va == 3) %>% 
  summarise(start_date = min(Date))
  #2021-10-05

intv_kor_v %>% 
  filter(va == 2) %>% 
  summarise(start_date = min(Date))
  #vaccines were available to anyone over the age of 16 yrs from 2021-08-09

intv_kor_v %>% 
  filter(va == 1) %>% 
  summarise(start_date = min(Date))

intv_kor_v %>% 
  summarise(start = min(Date), end = max(Date))
