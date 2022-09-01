source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/0803_parameters.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/0803_mod_sim.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/0803_obs.R')

pacman::p_load(ggpubr)
###########PLOT OBSERVED VS MODELLED DATA
#--Overall
cols <- c('observed' = 'light grey', 'modelled' = 'red')
ggplot()+
  geom_point(data = obs_t, aes(x=time, y=Dt_per_m, colour='observed'),alpha=0.7, size=1.5)+
  geom_line(data = output2_sim, aes(x=time, y=Dt_per_m, colour='modelled'))+
  scale_x_continuous(breaks=c(0, seq(0, 160, 25), 165))+
  labs(x = 'Time (day)', y = 'Confirmed daily incident COVID-19 cases\n(per million people)\n')+
  theme_classic2(base_size = 10)+
  scale_colour_manual(values = cols)


#--Lower tail
ggplot()+
  geom_point(data = obs_t, aes(x=time, y=daily), colour='red',alpha=0.5, size=1.5)+
  geom_line(data = output2_sim, aes(x=time, y=Dt_per_m))+
  labs(x = 'Time (day)', y = 'Reported total daily incidence')+
  theme_classic2()+
  coord_cartesian(xlim = c(0,50))

#--Upper tail
ggplot()+
  geom_point(data = obs_t, aes(x=time, y=daily), colour='red',alpha=0.5, size=1.5)+
  geom_line(data = output2_sim, aes(x=time, y=Dt_per_m))+
  labs(x = 'Time (day)', y = 'Reported total daily incidence')+
  theme_classic2()+
  coord_cartesian(xlim = c(100, 165))
