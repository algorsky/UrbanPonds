library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
tempdo = read_csv('data/tempdo.csv') 


ggplot(dplyr::filter(tempdo, pond == 'ELV')) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = DATE, size = 1.5)) +
  geom_path(aes(x = Temp_C, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  theme_bw()
  
ggplot(data = tempdo) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = DATE, size = 0.5)) +
  geom_path(aes(x = DO_mgL, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond)+
  scale_x_continuous(name = "Water Temperature (C)")+
  theme_bw()
