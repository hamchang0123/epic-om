pacman::p_load(tidyverse, rio, deSolve, reshape2)

#---Import Google Mobility dataset
ggl_mob_raw <- import('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/data/google_mobility.csv')

ggl_mob_kor <- ggl_mob_raw %>%
  filter(Code=='KOR') %>%
  mutate(Date = lubridate::mdy(Day)) %>%
  filter(Date > '2022-01-15' & Date < '2022-07-01') %>%
  dplyr::select(Date, grocery_and_pharmacy, residential, retail_and_recreation, transit_stations, workplaces)

#---Choose mobility index at workplaces
mob_work <- ggl_mob_kor %>%
  dplyr::select(workplaces) 

mob_work <- mob_work %>%
  mutate(time = seq(0:165)-1)

#---Analyse
mob_work %>%
  filter(time < 92) %>%
  summarise(pre_lifting_mean = mean(workplaces, na.rm=T), sd = sd(workplaces))
# pre_lifting_mean    sd
#     -6.003065 9.883675

mob_work %>%
  filter(workplaces == min(workplaces))
#  pre_lifting_max
#         -40
#  on day 19 

mob_work %>%
  filter(time > 91) %>%
  summarise(post_lifting_mean = mean(workplaces, na.rm=T), sd = sd(workplaces))
#  post_lifting_mean    sd
#        4.337838 5.562915

#---Plot
#----Google Mobility Index from day 0 to 165
ggplot() +
  geom_line(data = mob_work, aes(x=time, y=workplaces), size=0.5)+
  geom_vline(xintercept=91, linetype=3, alpha=0.5)+
  geom_segment(aes(x=0, xend=91, y=-6.003065, yend=-6.003065), linetype=2, alpha=0.8, colour = '#7570b3') +
  geom_segment(aes(x=92, xend=165, y=4.337838, yend=4.337838), linetype=2, alpha=0.8, colour = '#1b9e77')+
  geom_segment(aes(x=15, xend=25, y=-40, yend=-40), linetype=2, alpha=0.8, colour = '#d95f02')+
  theme_classic2() +
  annotate(geom = "text", x=160, y=2, label="(+4.338)", size=3, colour='#1b9e77')+
  annotate(geom = "text", x=80, y=-8, label="(-6.003)", size=3, colour = '#7570b3')+
  annotate(geom = "text", x=20, y=-42, label="(-40)", size=3, colour = '#d95f02')+
  annotate(geom = "text", x=75, y=-50, label="with c2.2", colour="grey", size=3, alpha=0.8)+
  annotate(geom = "text", x=150, y=-50, label="without c2.2", colour="grey", size=3, alpha=0.8)+
  ylab('Google Mobility Index')+
  xlab('Time (day)')+
  scale_x_continuous(breaks = c(0, 19, 91, 165))
