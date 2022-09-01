#--Read in baseline models
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no1.1.R')

source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1_long.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1up.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1down1.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_c3.1down2.R')

#--Read in models without a particular intervention source
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no3.1.R')

###Hypothetical: longer c3.1 effects
df_c3.1_long <- data.frame(
  indicator = c('cumulative case', 'peak case', 'peak time'),
  baseline_c3.1_long = c(cum_t_c3.1_long, pk_t_c3.1_long_case, pk_t_c3.1_long_time),
  no_c3.1 = c(cum_t_no_c3.1, pk_t_no_c3.1_case, pk_t_no_c3.1_time)
)

df_c3.1_long <- df_c3.1_long %>%
  mutate(
    e_c3.1_long = baseline_c3.1_long - no_c3.1
  ) %>%
  mutate(
    p_c3.1_long = e_c3.1_long / baseline_c3.1_long * 100
  )

df_c3.1_long[,2:4] <- round(df_c3.1_long[,2:4], 0)
df_c3.1_long[,5] <- round(df_c3.1_long[,5], 2)

rio::export(x = df_c3.1_long, file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/df_c3.1_long_0810.csv')

###Sensitivity analysis: differential c3.1 effects
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
cum_t_sim_c3.1up = cumt(output2_sim_c3.1up)
cum_t_sim_c3.1down1 = cumt(output2_sim_c3.1down1)
cum_t_sim_c3.1down2 = cumt(output2_sim_c3.1down2)

pk_t_sim_c3.1up_case = pkcase(output2_sim_c3.1up)
pk_t_sim_c3.1down1_case = pkcase(output2_sim_c3.1down1)
pk_t_sim_c3.1down2_case = pkcase(output2_sim_c3.1down2)

pk_t_sim_c3.1up_time = pktime(output2_sim_c3.1up)
pk_t_sim_c3.1down1_time = pktime(output2_sim_c3.1down1)
pk_t_sim_c3.1down2_time = pktime(output2_sim_c3.1down2)

df_sa_c3.1 <- data.frame(
  indicator = c('cumulative case', 'peak case', 'peak time'),
  baseline = c(cum_t, pk_t_case, pk_t_time),
  baseline_up = c(cum_t_sim_c3.1up, pk_t_sim_c3.1up_case, pk_t_sim_c3.1up_time),
  baseline_down1 = c(cum_t_sim_c3.1down1, pk_t_sim_c3.1down1_case, pk_t_sim_c3.1down1_time),
  baseline_down2 = c(cum_t_sim_c3.1down2, pk_t_sim_c3.1down2_case, pk_t_sim_c3.1down2_time),
  no_c3.1 = c(cum_t_no_c3.1, pk_t_no_c3.1_case, pk_t_no_c3.1_time)
)

df_sa_c3.1 <- df_sa_c3.1 %>%
  mutate(
    e_c3.1_base = baseline - no_c3.1,
    e_c3.1up = baseline_up - no_c3.1,
    e_c3.1down1 = baseline_down1 - no_c3.1,
    e_c3.1down2 = baseline_down2 - no_c3.1
  ) %>%
  mutate(
    p_c3.1_base = e_c3.1_base / baseline * 100,
    p_c3.1up = e_c3.1up / baseline_up * 100,
    p_c3.1down1 = e_c3.1down1 / baseline_down1 * 100,
    p_c3.1down2 = e_c3.1down2 / baseline_down2 * 100
  )

df_sa_c3.1[,11:14] <- round(df_sa_c3.1[,11:14], 2)
df_sa_c3.1[,2:10] <- round(df_sa_c3.1[,2:10], 0)

rio::export(x = df_sa_c3.1, file = 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/df_sa_c3.1_0810.csv')
