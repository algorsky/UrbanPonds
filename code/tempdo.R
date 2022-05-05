library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
tempdo <- read_csv('data/ancillary/tempdo.csv') 

tempdo_summer<- tempdo%>%
  filter(DATE< "2022-01-01")

#Specific Pond
ggplot(dplyr::filter(tempdo, pond == 'ELV')) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = DATE, size = 1.5)) +
  geom_path(aes(x = DO_mgL, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  theme_bw()

#Facet Wrap  
oxy<-ggplot(data = tempdo) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = DATE),size = 0.2) +
  geom_path(aes(x = DO_mgL, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond)+
  scale_x_continuous(name = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  theme_bw(base_size = 12)
ggsave("figures/oxy.png", width = 8, height = 6, units = 'in', oxy)
temp<-ggplot(data = tempdo) + 
  geom_point(aes(x = Temp_C, y = Depth, color = DATE),size = 0.2) +
  geom_path(aes(x = Temp_C, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond)+
  scale_x_continuous(name = ((expression("Temperature " ( degree*C)))))+
  theme_bw(base_size = 12)
ggsave("figures/temp.png", width = 8, height = 6, units = 'in', temp)
# Read in data 
winter <- read_csv('data/winter/temp_do.csv') 
winter_dates<- winter%>%
  group_by(DATE, pond)%>%
  summarize(pH = mean(pH))

ggplot(data = tempdo_summer) + 
  geom_point(aes(x = Temp_C, y = Depth, color = DATE),size = 0.2) +
  geom_path(aes(x = Temp_C, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond, scales = "free")+
  scale_x_continuous(name = ((expression("Temperature " ( degree*C)))))+
  theme_bw(base_size = 12)

