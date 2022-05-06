library(tidyverse)
library(devtools)
library(lubridate)
library(gridExtra)
library(patchwork)
library(scales)
library(patchwork)
library(broom)
#Gas Plots
# Read in data 
gas_summer = read_csv('data/gas/tidy.summer21_01.csv')
gas_winter = read_csv('data/gas/winter22/tidy.dat.out.winter22.csv')
extra = read_csv('data/ancillary/extra.csv')
tempdo <- read_csv('data/ancillary/tempdo.csv') 

macrophyte<- extra%>%
  mutate(Floating = ifelse(is.na(Floating), 0, Floating))%>%
  group_by(pond)%>%
  summarize(emergent = mean(Emergent),
            Floating = mean(Floating))

gas_summer<- gas_summer%>%
  mutate(season = "summer")%>%
  mutate(pond = replace(pond, pond == "HP", "HP-UW"))
gas_winter<- gas_winter%>%
  mutate(season = "winter")
depth<- extra%>%
  group_by(pond)%>%
  summarize(depth = mean(Zmax))

tempdo<- tempdo%>%
  mutate(season = ifelse(month(DATE) < 4, "winter", "summer"))%>%
  mutate(sampling = ifelse(day(DATE) %in% 12:14, 1,
                           ifelse(day(DATE) %in% 27:31, 2,
                                  ifelse(day(DATE) %in% 9:11, 3, NA))))

#Combine
gas_seasons<- rbind(gas_summer, gas_winter)
gas_seasons<-gas_seasons%>%
  left_join(depth, by = "pond")%>%
  left_join(macrophyte, by = "pond")%>%
  mutate(pond = fct_reorder(pond, depth.y))


gas_summary<- gas_seasons%>%
  group_by(season)%>%
  summarize(meanCO2 = mean(dissolvedCO2*1000000),
            meanCH4 = mean(dissolvedCH4*1000000),
            meanN2O = mean(dissolvedN2O*1000000),
            minCO2 = min(dissolvedCO2*1000000),
            minCH4 = min(dissolvedCH4*1000000),
            minN2O = min(dissolvedN2O*1000000),
            maxCO2 = max(dissolvedCO2*1000000),
            maxCH4 = max(dissolvedCH4*1000000),
            maxN2O = max(dissolvedN2O*1000000))

ggplot(gas_seasons)+
  geom_boxplot(aes(x = season, y = dissolvedCH4*1000000, group = season, fill = season, alpha = 0.3))+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  xlab("")+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/jasm/ch4_boxplot.png", width = 5, height = 7, units = 'in')

co2_plot<-ggplot(data = gas_seasons,aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/jasm/co2_full_figure.png", width = 11, height = 8.5, units = 'in', co2_plot)

ggplot(dplyr::filter(gas_seasons, pond == "HP-UW"), aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

n2o_arb<-ggplot(dplyr::filter(gas_seasons, pond == "LM-ARB"|pond == "UM-ARB"), aes(x = as.factor(pond), y = dissolvedN2O *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")


co2_figure<-co2_plot + co2_kp+ plot_layout(ncol=2,widths=c(4,1))
ggsave("figures/jasm/co2_figure.png", width = 11, height = 8.5, units = 'in', co2_figure)

ch4_plot<-ggplot((gas_seasons), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/jasm/ch4_full_figure.png", width = 11, height = 8.5, units = 'in', ch4_plot)

ch4_hp<-ggplot(dplyr::filter(gas_seasons, pond == "HP-UW"), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/jasm/co2_HP.png", width = 6, height = 8, units = 'in')

ch4_figure<-ch4_plot + ch4_kp + plot_layout(ncol=2,widths=c(4,1))

gas_arb<- co2_arb +ch4_arb+n2o_arb
ggsave("figures/gas_arb.png", width = 12, height = 8, units = 'in')

n2o_plot<-ggplot(gas_seasons, aes(x = as.factor(pond), y = dissolvedN2O *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Nitrous Oxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

#Temperature and Oxygen
#Specific Pond
ggplot(dplyr::filter(tempdo, pond == 'HP-UW')) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = as.factor(season), size = 1.5)) +
  geom_path(aes(x = DO_mgL, color = as.factor(season), y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(legend.position = "none")
ggsave("figures/jasm/O2_HP-UW.png", width = 8, height = 6, units = 'in')
ggplot(dplyr::filter(tempdo, pond == 'UM-ARB')) + 
  geom_point(aes(x = Temp_C, y = Depth, color = as.factor(season), size = 1.5)) +
  geom_path(aes(x = Temp_C, color = as.factor(season), y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  facet_wrap(~season, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")
ggsave("figures/jasm/Temp_ALC.png", width = 8, height = 6, units = 'in')

ggplot(dplyr::filter(tempdo, season == 'winter')) + 
 # geom_point(aes(x = Temp_C, y = Depth, color = as.factor(sampling)), size = 3) +
  geom_path(aes(x = Temp_C, y = Depth, group = interaction(pond, DATE), color = as.factor(sampling)))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(expression("Temperature " ( degree*C)))+
  scale_color_manual(name = "Sampling Event", values = c("steelblue1", "steelblue", "steelblue4"))+
  theme_bw(base_size = 20)

ggplot(dplyr::filter(tempdo, season == 'summer')) + 
  geom_point(aes(x = Temp_C, y = Depth, color = (DATE)), size = 3, alpha = 0.5) +
  geom_path(aes(x = Temp_C, y = Depth, group = interaction(pond, DATE), color = (DATE), alpha = 0.5))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(expression("Temperature " ( degree*C)))+
  theme_bw(base_size = 20)

ggplot(dplyr::filter(tempdo, season == 'winter')) + 
  geom_point(aes(x = Temp_C, y = Depth, color = DATE),size = 0.2) +
  geom_path(aes(x = Temp_C, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond, scales = "free")+
  scale_x_continuous(name = ((expression("Temperature " ( degree*C)))))+
  theme_bw(base_size = 20)

ggplot(dplyr::filter(tempdo, season == 'winter')) + 
  geom_point(aes(x = Temp_C, y = Depth, color = DATE),size = 0.2) +
  geom_path(aes(x = Temp_C, color = DATE, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond, scales = "free")+
  scale_x_continuous(limits = c(0, 6), name = ((expression("Temperature " ( degree*C)))))+
  theme_bw(base_size = 12)

ggplot(tempdo) + 
  geom_point(aes(x = Temp_C, y = Depth, color = season),size = 0.2) +
  geom_path(aes(x = Temp_C, color = season, y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  facet_wrap(~pond, scales = "free")+
  scale_x_continuous(name = ((expression("Temperature " ( degree*C)))))+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 12)
ggsave("figures/jasm/temp_facet.png", width = 10, height = 8, units = 'in')


surface_do<- tempdo%>%
  filter(ifelse(month(DATE) < 4, Depth == 0, Depth == 0.25))%>%
  rename(date = DATE)

gas_do<-gas_seasons%>%
  group_by(pond, date)%>%
  left_join(surface_do)%>%
  mutate(dissolvedCH4 = dissolvedCH4*1000000)%>%
  mutate(dissolvedCO2 = dissolvedCO2*1000000)%>%
  mutate(dissolvedN2O = dissolvedN2O*1000000)%>%
  filter(pond != "KP")

ch4_do<-ggplot(gas_do)+
  geom_point(aes(x = DO_mgL, y = dissolvedCH4, color = season), alpha = 0.3, size = 3)+
  #geom_smooth(aes(x = DO_mgL, y = dissolvedCH4, group = season, color = season))+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab(expression(paste("Surface O"[2], " (mg " , L^-1,")")))+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(legend.position = "")
co2_do<-ggplot(gas_do)+
  geom_point(aes(x = DO_mgL, y = dissolvedCO2, color = season), alpha = 0.3, size = 3)+
  #geom_smooth(aes(x = DO_mgL, y = dissolvedCO2, group = season, color = season))+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab(expression(paste("Surface O"[2], " (mg " , L^-1,")")))+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)

ch4_do+co2_do +plot_layout(guides = "collect")
ggsave("figures/jasm/O2_GHG.png", width = 16, height = 8, units = 'in')
fit_ch4<- gas_do%>%
  group_by(season)%>%
  do(glance(lm(dissolvedCH4~DO_mgL, .)))
glance(fit_winter)

ggplot(dplyr::filter(gas_seasons, season == "summer"))+
  geom_point(aes(x = Floating.x, y = dissolvedCH4), color = "darkgreen")+
  geom_point(aes(x = emergent.y, y = dissolvedCH4), color = "brown")
  

