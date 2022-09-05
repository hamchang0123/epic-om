pacman::p_load(tidyverse, rio, ggpubr, magrittr, gridExtra, grid, lattice, cowplot)

setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om/main')


#--Read in baseline models
source('./mod_sim.R')
source('./mod_sim_c4.3.R')
source('./mod_sim_c4.3_long.R')


#--Read in models without a particular intervention 
source('./mod_no1.1.R')
source('./mod_no4.4.R')
source('./mod_no4.3.R')

###c4.3 vs c4.3_long vs no_c4.3
df_comp_c4.3 <- data.frame(
  indicator = c('cumulative case', 'peak case', 'peak time'),
  baseline_c4.3 = c(cum_t_sim_c4.3, pk_t_sim_c4.3_case, pk_t_sim_c4.3_time),
  baseline_c4.3_long = c(cum_t_sim_c4.3_long, pk_t_sim_c4.3_long_case, pk_t_sim_c4.3_long_time),
  no_c4.3 = c(cum_t_no_c4.3, pk_t_no_c4.3_case, pk_t_no_c4.3_time)
) 

df_comp_c4.3 <- df_comp_c4.3 %>%
  mutate(
    e_c4.3 = baseline_c4.3 - no_c4.3,
    e_c4.3_long = baseline_c4.3_long - no_c4.3
    ) %>%
  mutate(
    p_c4.3 = e_c4.3 / baseline_c4.3 * 100,
    p_c4.3_long = e_c4.3_long / baseline_c4.3_long * 100
  )
  
df_comp_c4.3[,2:6] <- round(df_comp_c4.3[,2:6], 0)
df_comp_c4.3[,7:8] <- round(df_comp_c4.3[,7:8], 2)

rio::export(x = df_comp_c4.3, file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/df_comp_c4.3_0810.csv')

###c4.3_long vs c4.4
df_comp_c4.4_long_c4.3 <- data.frame(
  indicator = c('cumulative case', 'peak case', 'peak time'),
  baseline = c(cum_t, pk_t_case, pk_t_time),
  no_c4.4 = c(cum_t_no_c4.4, pk_t_no_c4.4_case, pk_t_no_c4.4_time),
  baseline_c4.3_long = c(cum_t_sim_c4.3_long, pk_t_sim_c4.3_long_case, pk_t_sim_c4.3_long_time),
  no_c4.3 = c(cum_t_no_c4.3, pk_t_no_c4.3_case, pk_t_no_c4.3_time)
) 

rio::export(x = df_comp_c4.4_long_c4.3, file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/df_comp_c4.4_long_c4.3_0810.csv')
