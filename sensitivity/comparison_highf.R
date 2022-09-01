pacman::p_load(tidyverse, rio, ggpubr, magrittr, gridExtra, grid, lattice)
#--Read in baseline models
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_highf.R')
#--Read in models without a particular intervention 
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no1.1_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no2.2_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no4.4_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no3.1_highf.R')

cumt <- function(x){
  cumt = cumsum(x$Dt_per_m)[166]
  return(as.numeric(cumt))
}
pkcase <- function(x){
  pk = as.data.frame(x %>% 
                       filter(Dt_per_m == max(Dt_per_m)))
  return(pk$Dt_per_m)
}
pktime <- function(x){
  pk = as.data.frame(x %>% 
                       filter(Dt_per_m == max(Dt_per_m)))
  return(pk$time)
}


cumt_base = cumt(output2_sim_highf)
cumt_no1.1 = cumt(output2_no_c1.1_highf)
cumt_no2.2 = cumt(output2_no_c2.2_highf)
cumt_no4.4 = cumt(output2_no_c4.4_highf)
cumt_no3.1 = cumt(output2_no_c3.1_highf)

pktime_base = pktime(output2_sim_highf)
pktime_no1.1 = pktime(output2_no_c1.1_highf)
pktime_no2.2 = pktime(output2_no_c2.2_highf)
pktime_no4.4 = pktime(output2_no_c4.4_highf)
pktime_no3.1 = pktime(output2_no_c3.1_highf)

pkcase_base = pkcase(output2_sim_highf)
pkcase_no1.1 = pkcase(output2_no_c1.1_highf)
pkcase_no2.2 = pkcase(output2_no_c2.2_highf)
pkcase_no4.4 = pkcase(output2_no_c4.4_highf)
pkcase_no3.1 = pkcase(output2_no_c3.1_highf)


df_comp_high <- data.frame(
  indicator = c('cumulative case', 'peak case', 'peak time'),
  baseline = c(cumt_base, pkcase_base, pktime_base),
  no_c1.1 = c(cumt_no1.1, pkcase_no1.1, pktime_no1.1),
  no_c2.2 = c(cumt_no2.2, pkcase_no2.2, pktime_no2.2),
  no_c4.4 = c(cumt_no4.4, pkcase_no4.4, pktime_no4.4),
  no_c3.1 = c(cumt_no3.1, pkcase_no3.1, pktime_no3.1))

df_comp_high <- df_comp_high %>% 
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
df_comp_high[,2:11] <- round(df_comp_high[,2:11], 0)
df_comp_high[,12:14] <- round(df_comp_high[,12:14], 3)

### Plots
p0_highf <- ggplot() + 
  geom_line(data = output2_no_c4.4_highf, aes(x=time, y=Dt_per_m, colour = "without the intervention"), linetype=1)+
  geom_line(data=output2_sim_highf, aes(x=time, y=Dt_per_m, colour = "with all interventions"),linetype=2)+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.2),
        legend.key.size = unit(1, "mm"),
        legend.text = element_text(size =  7),
        legend.title = element_text(size = 8, face = "bold"))

p0.0_highf <- ggplot()+
  theme_void()

yleft <- textGrob('Daily confirmed incident cases\n(per million people)', rot = 90, gp = gpar(fontsize = 10))
bottom <- textGrob('Time (day)', gp = gpar(fontsize = 10))

grid.arrange(p0_highf, p0.0_highf, p1.1_highf, p2.2_highf, p4.4_highf, p3.1_highf, nrow=3, ncol=2, left = yleft, bottom = bottom, respect = T)
