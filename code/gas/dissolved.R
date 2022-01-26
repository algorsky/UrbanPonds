library(tidyverse)
library(devtools)
library(lubridate)
library(gridExtra)
library(patchwork)
library(scales)


## Load the data from Github
# Read in data 
gas = read_csv('data/gas/tidy.summer21_01.csv')
gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gas$doy = yday(gas$date)

gd <- gas %>% 
  group_by(pond) %>%
  summarise(
    CO2 = mean(dissolvedCO2)*1000000,
    CH4 = mean(dissolvedCH4)*1000000,
    N2O = mean(dissolvedN2O)*1000000
  )

averages<- gas %>%
  filter(dissolvedCH4 > 0)%>%
  filter(dissolvedCO2 > 0)%>%
  filter(dissolvedN2O > 0)%>%
  group_by(pond)%>%
  summarise(
    CO2 = mean(dissolvedCO2)*1000000,
    CH4 = mean(dissolvedCH4)*1000000,
    N2O = mean(dissolvedN2O)*1000000
  )

co2<-ggplot(dplyr::filter(gas, pond == "LM-ARB" | pond == "UM-ARB"))+
  stat_summary(aes(x = pond, y = dissolvedCO2 *1000000), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = dissolvedCO2 *1000000), fun.data = mean_se, geom = "errorbar")+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  theme_bw()


ch4<-ggplot(dplyr::filter(gas, pond == "LM-ARB" | pond == "UM-ARB"))+
  stat_summary(aes(x = pond, y = dissolvedCH4 *1000000), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = dissolvedCH4 *1000000), fun.data = mean_se, geom = "errorbar")+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  theme_bw()

n2o<- ggplot(dplyr::filter(gas, pond == "LM-ARB" | pond == "UM-ARB"))+
  stat_summary(aes(x = pond, y = dissolvedN2O *1000000), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = dissolvedN2O *1000000), fun.data = mean_se, geom = "errorbar")+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  theme_bw()

library(patchwork)
plot<- co2 +ch4 +n2o

ggsave("figures/ARB_summer.png", width = 8, height = 6, units = 'in', plot)

extra = read_csv('data/extra.csv')
size = read_csv('data/pond_size.csv')

coverage<- extra%>%
  group_by(pond_id)%>%
  summarize(depth = mean(Zmax),
            emergent = mean(Emergent))

explore<- cbind(averages, coverage, size)

ggplot(explore)+
  geom_point(aes(x = SA_ha, y = N2O))
