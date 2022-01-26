library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
df = read_csv('data/carbon/carbon_summer21.csv')  %>% 
  mutate(sampleID = str_replace_all(sampleid, pattern = "2021_08_", replacement = "2021-08-")) %>% 
  separate(col = sampleID, into = c('pond','date'), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date))%>%
  mutate(TC = TIC +TOC)

df$pond<- as.factor(df$pond)


pondMean<- df%>%
  group_by(pond)%>%
  summarise(meanDIC = mean(DIC),
            sdDIC = sd(DIC),
            meanDOC = mean(DOC),
            sdDOC = sd(DOC),
            meanTC = mean(TC),
            sdTC = sd(TC))
ggplot(pondMean)+
  stat_summary(aes(x = pond, y = meanDOC), fun = mean, geom = "bar")
ggplot(df)+
  stat_summary(aes(x = pond, y = DOC), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = DOC), fun.data = mean_se, geom = "errorbar")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))

ggplot(df)+
  stat_summary(aes(x = pond, y = DIC), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = DIC), fun.data = mean_se, geom = "errorbar")+
  ylab("Dissolved Inorganic Carbon (mg/L)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))

ggplot(df)+
  stat_summary(aes(x = pond, y = TC), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = TC), fun.data = mean_se, geom = "errorbar")+
  ylab("Dissolved Inorganic Carbon (mg/L)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))
