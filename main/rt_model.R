setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om')
pacman::p_load(rio, tidyverse, ggpubr)
options(digits=1)

## Import dataset on modelled Rt 
df_rt <- import('./data/rt.csv')
  
## Calculate the effect of each intervention
e_rt <- df_rt %>%
  mutate(
    e_c1.1 = no_c1.1, e_c2.2 = no_c2.2, e_c4.4 = no_c4.4, e_c3.1 = no_c3.1, e_c4.3_long = no_c4.3_long
  )
    
e_rt$e_c1.1[1] = (e_rt$no_c1.1[1]-e_rt$baseline[1])/e_rt$no_c1.1[1]*100
e_rt$e_c1.1[2] = (e_rt$no_c1.1[2]-e_rt$baseline[2])/e_rt$no_c1.1[2]*100 
e_rt$e_c2.2[1] = (e_rt$no_c2.2[1]-e_rt$baseline[1])/e_rt$no_c2.2[1]*100 
e_rt$e_c2.2[2] = (e_rt$no_c2.2[2]-e_rt$baseline[2])/e_rt$no_c2.2[2]*100 
e_rt$e_c2.2[3] = (e_rt$no_c2.2[3]-e_rt$baseline[3])/e_rt$no_c2.2[3]*100 
e_rt$e_c2.2[4] = (e_rt$no_c2.2[4]-e_rt$baseline[4])/e_rt$no_c2.2[4]*100 
e_rt$e_c4.4[1] = (e_rt$no_c4.4[1]-e_rt$baseline[1])/e_rt$no_c4.4[1]*100 
e_rt$e_c4.4[2] = (e_rt$no_c4.4[2]-e_rt$baseline[2])/e_rt$no_c4.4[2]*100 
e_rt$e_c4.4[3] = (e_rt$no_c4.4[3]-e_rt$baseline[3])/e_rt$no_c4.4[3]*100 
e_rt$e_c3.1[4] = (e_rt$no_c3.1[4]-e_rt$baseline[4])/e_rt$no_c3.1[4]*100



e_rt$e_c4.3_long[1] = (e_rt$no_c4.3_long[1]-e_rt$baseline_c4.3_long[1])/e_rt$no_c4.3_long[1]*100 
e_rt$e_c4.3_long[2] = (e_rt$no_c4.3_long[2]-e_rt$baseline_c4.3_long[2])/e_rt$no_c4.3_long[2]*100 
e_rt$e_c4.3_long[3] = (e_rt$no_c4.3_long[3]-e_rt$baseline_c4.3_long[3])/e_rt$no_c4.3_long[3]*100    
e_rt$e_c4.3_long[4] = (e_rt$no_c4.3_long[4]-e_rt$baseline_c4.3_long[4])/e_rt$no_c4.3_long[4]*100

e_rt[,2:13] <- round(e_rt[,2:13], 1)

export(e_rt, 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/e_rt.csv')

## Dataset for plotting
df_rt_plt <- data.frame(
  time = c(0:165),
  rt = c(rep(df_rt$baseline[2], 22), 
         rep(df_rt$baseline[3], 22), 
         rep(df_rt$baseline[4], 48),
         rep(df_rt$baseline[5], 74)),
  owid = c(rep(1.6, 22),
           rep(1.6, 22),
           rep(1.0, 48),
           rep(0.7, 74)))

df_rt_plt <- df_rt_plt %>% 
  mutate(diff = owid - rt)

cols = c('Our World in Data' = 'light grey', 'Model' = 'black')

## Plot
ggplot(data = df_rt_plt) +
  geom_line(aes(x=time, y=rt, colour= 'Model'))+
  geom_line(aes(x=time, y =owid, colour='Our World in Data'), linetype=2)+
  scale_x_continuous(breaks = c(0,22,44, 92, 165))+
  scale_y_continuous(breaks = c(df_rt_plt$rt[c(21, 43, 92, 165)], df_rt_plt$owid[c(21, 43, 92, 165)]))+
  labs(x = 'Time (day)', y = 'Rt')+
  theme_classic2(base_size=10)+
  scale_color_manual(values = cols)+
  labs(colour = 'Estimates')
  
