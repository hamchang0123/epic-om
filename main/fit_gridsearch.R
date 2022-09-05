#####################
pacman::p_load(tidyverse, rio, ggpubr, magrittr, gridExtra, grid, lattice, cowplot)

setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om/main')


#####################
source('./parameters.R')
source('./mod_sim.R')
source('./obs.R')

#######################
k=seq(0, 1, 0.1)


frac_rep=seq(0, 1, 0.1)

store_estimates <- NULL

for(i in k){
  
  for(j in frac_rep){
    
    parameters['k'] <- i
    parameters['frac_rep'] <- j
    
    output_f <- as.data.frame(ode(y = initial, 
                                  times = times, 
                                  func = mod_sim,
                                  parms = parameters))
    
    output2_f <- output_f %>%
      mutate(
        D_y = Ic_y * parameters['frac_rep'],
        D_a = Ic_a * parameters['frac_rep'], 
        D_t = (Ic_y + Ic_a) * parameters['frac_rep']
      )
    
    L <- sum(dpois(obs_t$daily, lambda = output2_f$D_t, log=T))
    
    
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
  scale_x_continuous(breaks = seq(0, 0.2, 0.01))+
  scale_y_continuous(breaks = seq(0, 0.3, 0.01))

ggplot(data = df_sto_est, aes(x = k, y= frac_rep)) +
  geom_tile(aes(fill=L))+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = seq(0.070, 0.095, 0.005))+
  scale_y_continuous(breaks = seq(0.10, 0.15, 0.005))+
  theme_classic2(base_size = 10)+
  labs(x = '\nk', y = 'm\n')+
  geom_point(aes(x = 0.08, y = 0.11), size=1.5, shape = 4)

ggplot(data = df_sto_est, aes(x = k, y= frac_rep)) +
  geom_tile(aes(fill=L))+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = seq(0.077, 0.079, 0.0005))+
  scale_y_continuous(breaks = seq(0.100, 0.120, 0.005))+
  theme_classic2()+
  labs(x = '\nk', y = 'frac_rep\n')+
  geom_point(aes(x = 0.077, y = 0.111), size=1.5, shape = 4)
  