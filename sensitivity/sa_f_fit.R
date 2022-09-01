source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/obs.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters_lowf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_lowf.R')
pacman::p_load(ggpubr, tidyverse, RColorBrewer, scales, lattice, grid, gridExtra)

###########PLOT OBSERVED VS MODELLED DATA
#--Overall
hue_pal()(3)

cols <- c('f=0.5' = '#F8766D', 'f=0.35' = '#00BA38', 'f=0.65' = '#619CFF')

p_f <- ggplot()+
  geom_point(data = obs_t, aes(x=time, y=Dt_per_m), colour='grey', alpha = 0.5, size=1.5)+
  geom_line(data = output2_sim_highf, aes(x=time, y=Dt_per_m, colour = 'f=0.65'), linetype=2)+
  geom_line(data = output2_sim_lowf, aes(x=time, y=Dt_per_m, colour ='f=0.35'), linetype=2)+
  geom_line(data = output2_sim, aes(x=time, y=Dt_per_m, colour='f=0.5'))+
  scale_x_continuous(breaks=c(0, seq(0, 160, 25), 165))+
  labs(x = 'Time (day)', y = 'Confirmed daily incident COVID-19 cases\n(per million people)\n')+
  theme_classic2(base_size = 10)+
  theme(legend.key.size = unit(0.3, 'cm'), plot.margin = margin(t=0, r=0, b=0, l=0))+
  scale_colour_manual(values = cols)
p_f
p_l <- ggplot()+
  geom_point(data = obs_t, aes(x=time, y=Dt_per_m), colour='grey', alpha = 0.5, size=1.5)+
  geom_line(data = output2_sim_highf, aes(x=time, y=Dt_per_m, colour = 'f=0.65'), linetype=2)+
  geom_line(data = output2_sim_lowf, aes(x=time, y=Dt_per_m, colour ='f=0.35'), linetype=2)+
  geom_line(data = output2_sim, aes(x=time, y=Dt_per_m, colour='f=0.5'))+
  scale_x_continuous(breaks=c(0, seq(0, 160, 25), 165))+
  labs(x = 'Time (day)', y = 'Confirmed daily incident COVID-19 cases\n(per million people)\n')+
  theme_classic2(base_size = 10)+
  scale_colour_manual(values = cols)+
  theme(axis.title = element_blank(), 
        plot.title = element_text(size=10), 
        legend.position="None",
        plot.margin = margin(t=0, r=0, b=0, l=0))+
  coord_cartesian(xlim=c(0,55), ylim=c(0,1500))

p_h <- ggplot()+
  geom_point(data = obs_t, aes(x=time, y=Dt_per_m), colour='grey', alpha = 0.5, size=1.5)+
  geom_line(data = output2_sim_highf, aes(x=time, y=Dt_per_m, colour = 'f=0.65'), linetype=2)+
  geom_line(data = output2_sim_lowf, aes(x=time, y=Dt_per_m, colour ='f=0.35'), linetype=2)+
  geom_line(data = output2_sim, aes(x=time, y=Dt_per_m, colour='f=0.5'))+
  scale_x_continuous(breaks=c(0, seq(0, 160, 25), 165))+
  labs(x = 'Time (day)', y = 'Confirmed daily incident COVID-19 cases\n(per million people)\n')+
  theme_classic2(base_size = 10)+
  scale_colour_manual(values = cols)+
  theme(axis.title = element_blank(), 
        plot.title = element_text(size=10), 
        legend.position="None",
        plot.margin = margin(t=0, r=0, b=0, l=0))+
  coord_cartesian(xlim=c(110,165), ylim=c(0,200))


yleft <- textGrob('Daily confirmed incident cases\n(per million people)', rot = 90, gp = gpar(fontsize = 10))
bottom <- textGrob('Time (day)', gp = gpar(fontsize = 10))
p_lh <- grid.arrange(p_l, p_h, nrow=2, ncol=1, padding = unit(0, "line"), respect = T)

grid.arrange(p_f, p_lh, nrow=1, ncol=2, padding = unit(0, "line"),respect = T)
