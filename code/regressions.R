library(tidyverse)
library(lubridate)
library(patchwork)

# Read in data 
lc <- read_csv('data/ancillary/pond_landcover.csv')
extra<- read_csv('data/ancillary/extra.csv')
size<- read_csv('data/ancillary/pond_size.csv')

arb_lc <- read_csv('data/arboretum/landcover.csv') 
gas = read_csv('data/gas/tidy.summer21_01.csv')
gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gd <- gas %>% 
  group_by(pond) %>%
  summarise(
    CO2 = mean(dissolvedCO2)*1000000,
    CH4 = mean(dissolvedCH4)*1000000,
    N2O = mean(dissolvedN2O)*1000000
  )



lc<- lc%>%
  rename(pond = Site.Code)
gas_bind<- merge(lc, gd, by = "pond")
gas_bind_extra<- merge(gas_bind, extra, by = "pond")

development<-ggplot(lc)+
  geom_point(aes(x = pond, y = Developed200m), size = 4)+
  ylab("Developed 200m Buffer")+
  xlab("")+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

age<-ggplot(lc)+
  geom_point(aes(x = pond, y = Pond.Age), size = 4)+
  ylab("Pond Age")+
  xlab("")+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

depth<-ggplot(extra)+
  geom_boxplot(aes(x = pond, y = Zmax))+
  ylab("Sampling Depth (m)")+
  xlab("Pond")+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

size<-ggplot(size)+
  geom_point(aes(x = Pond_Id, y = SA_ha), size = 4)+
  ylab("Surface Area (ha)")+
  xlab("Pond")+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

combo_pond<- development   + depth + age+ size 
ggsave("figures/combo_pond.png", width = 11, height = 8.5, units = 'in', combo_pond)



ggplot(gas_bind_extra)+
  geom_boxplot(aes(x = pond, y = Algae))+
  ylab("Algae Percent Cover")+
  xlab("Pond")+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(gas_bind_extra)+
  geom_boxplot(aes(x = pond, y = Floating))+
  ylab("Floating Percent Cover")+
  xlab("Pond")+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




