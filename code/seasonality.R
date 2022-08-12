library(tidyverse)
library(devtools)
library(lubridate)
library(patchwork)
# Read in data for TSS
tss_winter = read_csv('data/winter/TSS.csv')
tss_summer = read_csv('data/ancillary/TSS.csv')

tss<- rbind(tss_summer, tss_winter)

tss_summary<- tss%>%
  group_by(year(Date))%>%
  summarize(medianTSS = median(TSS_gL* 1000),
            minTSS = min(TSS_gL* 1000),
            maxTSS = max(TSS_gL* 1000))

TSS_plot<-ggplot(tss)+
  geom_col(aes(x = factor(format(Date, format = "%m-%d")), y = TSS_gL * 1000, 
               fill = as.factor(year(Date))), show.legend = FALSE)+
  xlab("")+
  ylab(expression(paste("TSS (mg", L^-1,")")))+
  facet_wrap(~Pond, scales = "free")+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/TSS_seasonal.png", width = 11, height = 8.5, units = 'in', TSS_plot)


ggplot(dplyr::filter(tss, Pond == "KP"))+
  geom_col(aes(x = factor(format(Date, format = "%m-%d")), y = TSS_gL * 1000, 
               fill = as.factor(year(Date))), show.legend = FALSE)+
  xlab("")+
  ylab(expression(paste("TSS (mg", L^-1,")")))+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  facet_wrap(~Pond, scales = "free")+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/jasm/TSS_KP.png", width = 8, height = 6, units = 'in')

tss_statistics<- tss%>%
  group_by(Pond, year(Date))%>%
  summarise(mean_TSS_mgL = mean(TSS_gL * 1000),
            OM_perc = mean(OM_perc),
            sd_TSS = sd(TSS_gL * 1000))

ggplot(tss_statistics)+
  