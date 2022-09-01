setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin')
options(install.lock =F)
pacman::p_load(rio, tidyverse, vistime, ggpubr, lubridate)

timeline_data <- data.frame(
  event = c('Recommending\nschool closure', 'Requiring workplace closure', 'Restricting gatherings to 10 people or fewer','Requiring public events cancellation', 'Recommending public events cancellation', 'None'),
  start =c(rep('2022-01-16',4), "2022-03-01", "2022-04-18"),
  end = c("2022-02-06", "2022-04-17", "2022-04-17", "2022-02-28", "2022-04-17", "2022-06-30"),
  group = "Measures"
)

vis_dt <- vistime_data(timeline_data)
vis_dt$y[5]=1

gg_vistime(vis_dt, optimize_y = F)+
  theme_classic2(base_size = 14)+
  scale_x_datetime(
    breaks = c(as.POSIXct(vis_dt$start[1]), as.POSIXct(vis_dt$end[c(1,2,4,6)])),
    date_labels = "%d-%m-%y")+
  geom_vline(xintercept = as.POSIXct(vis_dt$end[1]), linetype=3, alpha=0.5, colour='grey')+
  geom_vline(xintercept = as.POSIXct(vis_dt$end[4]), linetype=3, alpha=0.5, colour='grey')+
  geom_vline(xintercept = as.POSIXct(vis_dt$end[5]), linetype=3, alpha=0.5, colour='grey')+
  geom_vline(xintercept = as.POSIXct(vis_dt$end[6]), linetype=3, alpha=0.5, colour='grey')+
  geom_vline(xintercept = as.POSIXct(vis_dt$start[1]), linetype=3, alpha=0.5, colour='grey')


str(gg)
