library(tidyverse)
library(lubridate)

# Read in data 
hobo <- read_csv('data/hobo/summer2021.csv') 
hobo<- hobo%>%
  mutate(datetime = as.POSIXct(datetime, tz = "Central"))%>%
  mutate(hourtime = ifelse((hour(datetime)< 6) | (hour(datetime) > 19), "night","day"))

week<- hobo%>%
  filter(datetime < "2021-07-17" & datetime > "2021-07-10")


color<- 


surface<-ggplot(hobo)+
  geom_line(aes(x = datetime, y = DO_mgL))+
  ylab("Surface Oxygen (mg/L)")+
  facet_wrap(~pond)+
  scale_fill_manual(breaks = c())
  theme_bw()

ggsave("figures/HOBO_DO.png", width = 11, height = 8.5, units = 'in', surface)


#One day
ggplot(dplyr::filter(hobo, as.Date(datetime) == "2021-07-28"))+
  geom_point(aes(x = datetime, y = DO_mgL))+
  ylab("Oxygen (mg/L)")+
  facet_wrap(~pond)+
  theme_bw()

ggplot(week)+
  geom_point(aes(x = datetime, y = DO_mgL, color = hourtime))+
  ylab("Surface Oxygen (mg/L)")+
  facet_wrap(~pond)+
  theme_bw()

ggsave("figures/HOBO_DO_week.png", width = 11, height = 8.5, units = 'in', week)
