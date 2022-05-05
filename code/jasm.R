#Gas Plots
# Read in data 
gas_summer = read_csv('data/gas/tidy.summer21_01.csv')
gas_winter = read_csv('data/gas/winter22/tidy.dat.out.winter22.csv')
extra = read_csv('data/ancillary/extra.csv')
tempdo <- read_csv('data/ancillary/tempdo.csv') 

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
  mutate(pond = fct_reorder(pond, depth))
co2_plot<-ggplot(dplyr::filter(gas_seasons, pond != "KP"), aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

co2_kp<-ggplot(dplyr::filter(gas_seasons, pond == "KP"), aes(x = as.factor(pond), y = dissolvedCO2 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

co2_figure<-co2_plot + co2_kp+ plot_layout(ncol=2,widths=c(4,1))

ch4_plot<-ggplot(dplyr::filter(gas_seasons, pond != "KP"), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

ch4_kp<-ggplot(dplyr::filter(gas_seasons, pond == "KP"), aes(x = as.factor(pond), y = dissolvedCH4 *1000000, fill = season, alpha = 0.3))+
  geom_boxplot(outlier.size = 0)+
  geom_point(pch = 21, position = position_jitterdodge())+
  geom_hline(yintercept = 0, alpha = 0.8)+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab("")+
  scale_fill_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")

ch4_figure<-ch4_plot + ch4_kp + plot_layout(ncol=2,widths=c(4,1))

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
ggplot(dplyr::filter(tempdo, pond == 'KP')) + 
  geom_point(aes(x = DO_mgL, y = Depth, color = as.factor(season), size = 1.5)) +
  geom_path(aes(x = DO_mgL, color = as.factor(season), y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw()+
  theme(legend.position = "none")

ggplot(dplyr::filter(tempdo, pond == 'KP')) + 
  geom_point(aes(x = Temp_C, y = Depth, color = as.factor(season), size = 1.5)) +
  geom_path(aes(x = Temp_C, color = as.factor(season), y = Depth, group = DATE))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw()+
  theme(legend.position = "none")

ggplot(dplyr::filter(tempdo, season == 'winter')) + 
 # geom_point(aes(x = Temp_C, y = Depth, color = as.factor(sampling)), size = 3) +
  geom_path(aes(x = Temp_C, y = Depth, group = interaction(pond, DATE), color = as.factor(sampling)))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(expression("Temperature " ( degree*C)))+
  scale_color_manual(name = "Sampling Event", values = c("steelblue1", "steelblue", "steelblue4"))+
  theme_bw(base_size = 20)

ggplot(dplyr::filter(tempdo, season == 'summer')) + 
  geom_point(aes(x = Temp_C, y = Depth, color = (DATE)), size = 3) +
  geom_path(aes(x = Temp_C, y = Depth, group = interaction(pond, DATE), color = (DATE)))+
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
  scale_x_continuous(name = ((expression("Temperature " ( degree*C)))))+
  theme_bw(base_size = 12)

surface_do<- tempdo%>%
  filter(ifelse(month(DATE) < 4, Depth == 0, Depth == 0.25))%>%
  rename(date = DATE)

gas_do<-gas_seasons%>%
  group_by(pond, date)%>%
  left_join(surface_do)%>%
  mutate(dissolvedCH4 = dissolvedCH4*1000000)%>%
  mutate(dissolvedCO2 = dissolvedCO2*1000000)%>%
  mutate(dissolvedN2O = dissolvedN2O*1000000)

ggplot(dplyr::filter(gas_do, pond != "KP"))+
  geom_point(aes(x = DO_mgL, y = dissolvedCH4, color = season), alpha = 0.3, size = 3)+
 # geom_smooth(aes(x = DO_mgL, y = dissolvedCH4))+
  ylab(expression(paste('Dissolved Methane (',mu,'mol ','L'^-1, ')')))+
  xlab(expression(paste("Surface O"[2], " (mg " , L^-1,")")))+
  scale_color_manual(values = c("forestgreen", "blue2"))+
  theme_bw(base_size = 20)


