library(tidyverse)
library(devtools)
library(lubridate)
library(patchwork)
# Read in data for TSS
tss_winter = read_csv('data/winter/TSS.csv')
tss_summer = read_csv('data/ancillary/TSS.csv')

tss<- rbind(tss_summer, tss_winter)

TSS_plot<-ggplot(tss)+
  geom_col(aes(x = factor(format(Date, format = "%m-%d")), y = TSS_gL * 1000, 
               fill = as.factor(year(Date))), show.legend = FALSE)+
  xlab("")+
  ylab(expression(paste("TSS (mg", L^-1,")")))+
  facet_wrap(~Pond, scales = "free")+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/TSS_seasonal.png", width = 11, height = 8.5, units = 'in', TSS_plot)
