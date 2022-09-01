options("install.lock"=FALSE)
pacman::p_load(deSolve, tidyverse, ggpubr, reshape2, scales, latex2exp)

source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1up.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1down1.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1down2.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no3.1.R')

##### Sensitivity analysis on the effect of c3.1
cols <- c('higher (12)'= "#00BA38",
          'base (15)'="#F8766D", 
          'lower (18)'="#619CFF")

title <- expression('The effect of' ~ M[rec]^E ~'(maximum other contacts)')

ggplot() + 
  #geom_line(data = output2_no_c3.1, aes(x=time, y=Dt_per_m, colour = 'base'))+
  geom_line(data=output2_sim_c3.1up, aes(x=time, y=Dt_per_m, colour = 'higher (12)'), alpha=0.8)+ 
  geom_line(data=output2_sim, aes(x=time, y=Dt_per_m, colour = 'base (15)'), alpha=0.6)+
  geom_line(data=output2_sim_c3.1down1, aes(x=time, y=Dt_per_m, colour = 'lower (18)'), alpha=0.8)+
  labs(x = 'Time (day)', y = 'Daily confirmed incident cases\n(per million people)\n')+
  scale_x_continuous(breaks=c(0, seq(0, 160, 25), 165))+
  #scale_y_continuous(breaks = seq(0, 600000, 100000))+
  theme_classic2(base_size =12)+
  scale_color_manual(values = cols)+
  labs(colour = title)

p3.1updown 

