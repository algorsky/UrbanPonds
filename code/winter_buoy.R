library(tidyverse)
library(lubridate)

# Read in data 
buoy <- read_csv('hobo/HP_Winter22.csv') 
buoy<- buoy%>%
  mutate(DateTime_temp = as.POSIXct(DateTime_temp, format="%m/%d/%y %H:%M"))%>%
  mutate(Temp_C = (Temp_F - 32) * (5/9))



ggplot(dplyr::filter(buoy, (DateTime_temp) > "2022-01-13"))+
  geom_line(aes(x = DateTime_temp, y = Cond_microS_cm))+
  geom_line(aes(x = DateTime_temp, y = Light_lum_ft2),  color = "orange")+
  geom_line(aes(x = DateTime_temp, y = Temp_C * 100), color = "blue")+
  scale_y_continuous(name = "Conductivity (µS/cm)",
    sec.axis = sec_axis( trans=~./100, name="Temperature (C)"))+
  xlab("")+
  theme_bw()+
    theme( axis.title.y.right = element_text(color = "blue"))
ggsave('data/hobo/winter_buoy_ARB.jpg')


ggplot(buoy)+

  theme_bw()


ggplot(dplyr::filter(buoy, (DateTime_temp) > "2022-01-13"))+
  
  theme_bw()
