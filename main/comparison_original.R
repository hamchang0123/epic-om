pacman::p_load(tidyverse, rio, ggpubr, magrittr, gridExtra, grid, lattice)
#--Read in baseline models
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1_long.R')
#--Read in models without a particular intervention 
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no1.1.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no2.2.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no4.4.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no3.1.R')


df_comp_t <- data.frame(
  indicator = c('cumulative case', 'peak case', 'peak time'),
  baseline = c(cum_t, pk_t_case, pk_t_time),
  no_c1.1 = c(cum_t_no_c1.1, pk_t_no_c1.1_case, pk_t_no_c1.1_time),
  no_c2.2 = c(cum_t_no_c2.2, pk_t_no_c2.2_case, pk_t_no_c2.2_time),
  no_c4.4 = c(cum_t_no_c4.4, pk_t_no_c4.4_case, pk_t_no_c4.4_time),
  no_c3.1 = c(cum_t_no_c3.1, pk_t_no_c3.1_case, pk_t_no_c3.1_time)
)


df_comp_t <- df_comp_t %>% 
  mutate(
    e_c1.1 = baseline - no_c1.1,
    e_c2.2 = baseline - no_c2.2,
    e_c4.4 = baseline - no_c4.4,
    e_c3.1 = baseline - no_c3.1
  ) %>% 
  mutate(
    p_c1.1 = e_c1.1 / baseline * 100,
    p_c2.2 = e_c2.2 / baseline * 100,
    p_c4.4 = e_c4.4 / baseline * 100,
    p_c3.1 = e_c3.1 / baseline * 100
  )
df_comp_t[,2:11] <- round(df_comp_t[,2:11], 0)
df_comp_t[,12:14] <- round(df_comp_t[,12:14], 3)

rio::export(x = df_comp_t, file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/df_comp_t_0810.csv')


### Plots
p0 <- ggplot() + 
  geom_line(data = output2_no_c4.4, aes(x=time, y=Dt_per_m, colour = "without the intervention"), linetype=1)+
  geom_line(data=output2_sim, aes(x=time, y=Dt_per_m, colour = "with all interventions"),linetype=2)+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.2),
        legend.key.size = unit(1, "mm"),
        legend.text = element_text(size =  7),
        legend.title = element_text(size = 8, face = "bold"))

p0.0 <- ggplot()+
  theme_void()

yleft <- textGrob('Daily confirmed incident cases\n(per million people)', rot = 90, gp = gpar(fontsize = 10))
bottom <- textGrob('Time (day)', gp = gpar(fontsize = 10))

grid.arrange(p0, p0.0, p1.1, p2.2, p4.4, p3.1, nrow=3, ncol=2, left = yleft, bottom = bottom, respect = T)
