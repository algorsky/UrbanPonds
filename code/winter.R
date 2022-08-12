library(tidyverse)
library(devtools)
library(lubridate)
library(patchwork)
# Read in data for winter ice
icesnow = read_csv('data/winter/ice.csv')
ice<- icesnow%>%
  select(Pond, Date, avsnow, blackice, whiteice)%>%
  pivot_longer(cols = c("avsnow","blackice", "whiteice"),
               names_to = "type",
               values_to = "cm")

ice_snow<- ice %>%
  mutate(Year = year(Date))%>%
  group_by(factor(Year))%>%
  mutate(doy = yday(Date))%>%
  mutate(Month = month(Date))

ice_snow$type = factor(ice_snow$type , levels = c("avsnow", "whiteice", "blackice"))

ice<-ggplot(ice_snow, aes(x = as.Date(doy, origin = as.Date('2021-01-01')), y = cm, fill = type))+
  geom_bar(stat = "identity", color = "black", width = 6)+
  scale_x_date(labels = date_format("%b"))+
  ylab("Thickness(cm)")+
  xlab("Sampling Month")+
  ylim(0, 50)+
  scale_fill_manual(values = c("white",'gray88','gray4'), name = "") +
  facet_wrap(~Pond)+
  theme_bw(base_size = 12)

ggsave("figures/ice.png", width = 8, height = 6, units = 'in', ice)


# Read in data for temp_do
temp_do = read_csv('data/winter/temp_do.csv')

temp_do<- temp_do%>%
  mutate(sampling = ifelse(DATE > "2022-01-11" & DATE < "2022-01-15", 1,
                           ifelse(DATE > "2022-01-26"& DATE < "2022-02-01", 2,3)))


