#--SET UP
#---Load packages
options(digits=2, 'install.lock' = F)
pacman::p_load(tidyverse, rio, deSolve, reshape2, ggpubr, scales, ggbreak)

#---Load scripts
setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om/main')
source('./parameters.R')
source('./mod_sim.R')
source('./mod_no4.4.R')

#--DATASET
#---Create a table needed to calculate overshoot
df <- data.frame(
  time = output_sim$time,
  Ic1 = output_sim$Ic_a+output_sim$Ic_y,
  R1 = output_sim$R_a+output_sim$R_y,
  Ic2 = output_no_c4.4$Ic_a+output_no_c4.4$Ic_y,
  R2 = output_no_c4.4$R_a + output_no_c4.4$R_y
)

#---Change the wide table to a long one
df_long <- melt(as.data.frame(df), id = 'time')

#--CALCULATION
#---Calculate overshoot when all measures are in place
f1 <- max(df$R1)
  #final size of epidemic (cumulative cases)

h1 <- df %>% 
  filter(Ic1 == max(Ic1))
  
h1 <- as.numeric(h1['R1'])
  #number of clinically infectious individuals at herd immunity threshold (HIT)

o1 <- f1-h1
  #overshoot

#---Calculate overshoot when c4.4 is lifted
f2 <- max(df$R2)

h2 <- df %>% 
  filter(Ic2 == max(Ic2))

h2 <- as.numeric(h2['R2'])

o2 <- f2-h2

#---Calculate how much c4.4 reduced the overshoot of clinically infectious individuals 
(o2 - o1)/1E6
  #reduced nearly 4 per million people

#--PLOT
ggplot(data = df_long, aes(x = time, y = value/1E6, group=variable, colour = variable))+
  geom_line()+
  geom_vline(xintercept=49, linetype=3, alpha = 0.5)+
  geom_vline(xintercept=65, linetype=3, alpha = 0.5)+
  geom_hline(yintercept=h1/1E6, linetype=1, colour = '#F8766D', alpha = 0.6)+
  geom_hline(yintercept=h2/1E6, linetype=1, colour = "#00BFC4", alpha = 0.6)+
  geom_hline(yintercept=f1/1E6, linetype=1, colour = '#F8766D', alpha = 0.6)+
  geom_hline(yintercept=f2/1E6, linetype=1, colour = "#00BFC4", alpha = 0.6)+
  theme_classic2(base_size=12)+
  scale_x_continuous(breaks = c(0, 49, 65, 165))+
  labs(x = 'Time (day)', y = 'Number of individuals per million people',
       colour = 'Key')+
  scale_y_break(c(10, 120), scales = 1.5)