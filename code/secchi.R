library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
secchi = read_csv('data/secchi.csv') 



#Facet Wrap  
ggplot(data = secchi) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = DATE),size = 0.2))



+
  geom_path(aes(x = DO_mgL, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond)+
  scale_x_continuous(name = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  theme_bw()