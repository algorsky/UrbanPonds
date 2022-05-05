library(tidyverse)
library(lubridate)
library(anytime)
library(dplyr)

# Read in data 
chloro = read_csv('data/chlorophyll/chloro_all.csv')  %>% 
  filter(sample_id != "BLA")%>%
  separate(col = sample_id, into = c('pond','date'), sep = "_", remove = FALSE)%>%
  mutate(date = ymd(date))%>%
  mutate(day = format(date, format = "%m-%d"))

chloro_summary<- chloro%>%
  group_by(year(date))%>%
  summarize(medianCh = median(chla),
            minCh = min(chla),
            maxCh = max(chla))

chloro_plot<-ggplot(chloro)+
  geom_col(aes(x = factor((day)), y = chla, width = 0.5, fill = as.factor(year(date))), show.legend = FALSE)+
  xlab("")+
  ylab(expression(paste("Surface Chl a (",  mu,"g ", L^-1,")")))+
 # scale_fill_manual(values = c('gray','lightblue4','gold')) +
  facet_wrap(~pond, scales = "free")+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dplyr::filter(chloro, pond == "KP"))+
  geom_col(aes(x = factor((day)), y = chla, width = 0.5, fill = as.factor(year(date))), show.legend = FALSE)+
  xlab("")+
  ylab(expression(paste("Surface Chl a (",  mu,"g ", L^-1,")")))+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  facet_wrap(~pond, scales = "free")+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("figures/jasm/chloro_KP.png", width = 8, height = 6, units = 'in')

# Read in data 
tss = read_csv('data/TSS.csv')

tss_plot<-ggplot(tss)+
  geom_col(aes(x = factor(format(Date, format = "%m-%d")), y = TSS_gL * 1000))+
  xlab("")+
  ylim(0, 82)+
  ylab(expression(paste("TSS (mg", L^-1,")")))+
  facet_wrap(~Pond, scales = "free")+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/TSS.png", width = 8, height = 6, units = 'in', tss_plot)
