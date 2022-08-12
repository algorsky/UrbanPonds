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

#write_csv(df, 'data/carbon/summer21.csv')

carbon<- df%>%
  select(pond, date, DIC, DOC)%>%
  pivot_longer(cols = c("DIC","DOC"),
               names_to = "type",
               values_to = "value")
means.long<- melt(carbon, id.vars = "pond")
  
ggplot(df) +
  stat_summary(aes(x = pond, y = DOC), fun = mean, geom = "bar")+
  stat_summary(aes(x = pond, y = DIC), fun = mean, geom = "bar", position = "dodge")
  geom_bar(stat = "identity", position="dodge")+
  ylab(expression(paste('Carbon (mg','L'^-1, ')')))+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
cols<- c("DIC" = "grey80", "DOC" = "black")
carbon<-ggplot(carbon, aes(x=pond, y=value, color = factor(type), fill=factor(type))) + 
    stat_summary(fun=mean, geom="bar",position=position_dodge(1)) + 
    scale_color_manual("Carbon",values = cols, aesthetics = c("colour", "fill"))+
  ylab(expression(paste('Carbon (mg','L'^-1, ')')))+
  stat_summary(fun.min=min,fun.max=max,geom="errorbar",
               color="black",position=position_dodge(1), width=.2)+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("figures/carbon.png", width = 8, height = 6, units = 'in', carbon)

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
