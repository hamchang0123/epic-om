#####################
pacman::p_load(rio, tidyverse, ggpubr)

#####################
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim_highf.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/obs.R')

#######################
k=seq(0.065, 0.075, 0.001)

frac_rep=seq(0.10, 0.12, 0.001)

store_estimates <- NULL

for(i in k){
  
  for(j in frac_rep){
    
    parameters_highf['k'] <- i
    parameters_highf['frac_rep'] <- j
    
    output_f_highf <- as.data.frame(ode(y = initial, 
                                  times = times, 
                                  func = mod_sim_highf,
                                  parms = parameters_highf))
    
    output2_f_highf <- output_f_highf %>%
      mutate(
        D_y = Ic_y * parameters_highf['frac_rep'],
        D_a = Ic_a * parameters_highf['frac_rep'], 
        D_t = (Ic_y + Ic_a) * parameters_highf['frac_rep']
      )
    
    L <- sum(dpois(obs_t$daily, lambda = output2_f_highf$D_t, log=T))
    
    
    store_estimates <- rbind(store_estimates, c(L,i,j))
    
    colnames(store_estimates) <- c('L', 'k', 'frac_rep')
    
    df_sto_est <- as.data.frame(store_estimates) 
    
    
  }
  
}

df_sto_est %>%
  filter(L == max(L))


ggplot(data = df_sto_est, aes(x = k, y= frac_rep)) +
  geom_tile(aes(fill=L))+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 1, 0.1))

ggplot(data = df_sto_est, aes(x = k, y= frac_rep)) +
  geom_tile(aes(fill=L))+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = seq(0.065, 0.075, 0.001))+
  scale_y_continuous(breaks = seq(0.10, 0.12, 0.001))

ggplot(data = df_sto_est, aes(x = k, y= frac_rep)) +
  geom_tile(aes(fill=L))+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = seq(0.07, 0.09, 0.001))+
  scale_y_continuous(breaks = seq(0.10, 0.15, 0.005))+
  theme_classic2(base_size = 10)+
  labs(x = '\nk', y = 'm\n')+
  geom_point(aes(x = 0.08, y = 0.11), size=1.5, shape = 4)

ggplot(data = df_sto_est, aes(x = k, y= frac_rep)) +
  geom_tile(aes(fill=L))+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = seq(0.077, 0.079, 0.0005))+
  scale_y_continuous(breaks = seq(0.10, 0.12, 0.001))+
  theme_classic2()+
  labs(x = '\nk', y = 'frac_rep\n')+
  geom_point(aes(x = 0.077, y = 0.111), size=1.5, shape = 4)
  