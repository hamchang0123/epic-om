options(digits=2, 'install.lock' = F)

pacman::p_load(tidyverse, rio, deSolve, reshape2, ggpubr, scales, ggbreak)

source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_sim.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/mod_no4.4.R')

df <- data.frame(
  time = output_sim$time,
  Ic1 = output_sim$Ic_a+output_sim$Ic_y,
  R1 = output_sim$R_a+output_sim$R_y,
  Ic2 = output_no_c4.4$Ic_a+output_no_c4.4$Ic_y,
  R2 = output_no_c4.4$R_a + output_no_c4.4$R_y
)

df_long <- melt(as.data.frame(df), id = 'time')

f1 <- max(df$R1)

h1 <- df %>% 
  filter(Ic1 == max(Ic1))

h1 <- as.numeric(h1['R1'])

o1 <- f1-h1

f2 <- max(df$R2)

h2 <- df %>% 
  filter(Ic2 == max(Ic2))

h2 <- as.numeric(h2['R2'])

o2 <- f2-h2

(o2 - o1)/1E6
#nearly 4 per million people

ggplot(data = df_long, aes(x = time, y = value/1E6, group=variable, colour = variable))+
  geom_line()+
  geom_vline(xintercept=49, linetype=3, alpha = 0.5)+
  geom_vline(xintercept=65, linetype=3, alpha = 0.5)+
  geom_hline(yintercept=h1/1E6, linetype=1, colour = '#F8766D', alpha = 0.6)+
  geom_hline(yintercept=h2/1E6, linetype=1, colour = "#00BFC4", alpha = 0.6)+
  geom_hline(yintercept=f1/1E6, linetype=1, colour = '#F8766D', alpha = 0.6)+
  geom_hline(yintercept=f2/1E6, linetype=1, colour = "#00BFC4", alpha = 0.6)+
  theme_classic2(base_size=8)+
  scale_x_continuous(breaks = c(0, 49, 65, 165))+
  labs(x = 'Time (day)', y = 'Number of individuals per million people',
       colour = 'Key')+
  scale_y_break(c(10, 120), scales = 1.5)



