library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
secchi = read_csv('data/ancillary/secchi.csv') 

depth<- secchi%>%
  group_by(pond_id)%>%
  summarise(max = mean(Zmax))

ggplot(secchi)+
  stat_summary(aes(x = as.factor(pond_id), y = Secchi_Up), fun = mean, geom = "bar")+
  stat_summary(aes(x = as.factor(pond_id), y = Secchi_Up), fun.data = mean_se, geom = "errorbar")+
  ylab("Secchi Depth (m)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))

#Facet Wrap  
ggplot(data = secchi) + 
  geom_point(aes(x = as.factor(pond_id), y = Secchi_Up))



+
  geom_path(aes(x = DO_mgL, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond)+
  scale_x_continuous(name = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  theme_bw()