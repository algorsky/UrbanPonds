### working with 2018 EPC pilot experiment results with the goal of refining calculations for 2019

getwd()
setwd('~/Desktop/JENNA_EPC')

epcRAW<-read.csv("data/sedimentPCl/EPC_22_clean.csv")
View(epcRAW)

# Calculations
  #1. Dry mass equivalent of fresh sediment used (used average moisture content for 0-4cm). Units=g
drymass_g<-epcRAW$Mass_sediment_fresh_g
  #2. Change in SRP concentration: Initial SRP solution - final SRP concentration post shaking. Units=mg/L
deltaSRPconc<-epcRAW$Initial_SRP_mgL-epcRAW$Final_SRP_mgL

  #3 Converting solution volume from ml to L
Solution_Volume_L<-epcRAW$Solution_Volume_mL/1000

  #4. Calculate change in SRP as mass: deltaSRPconc (mg/L) / volume of solution used (L). Units=mg
deltaSRPmass<-deltaSRPconc/Solution_Volume_L
  #5. Calculate the change in SRP mass per gram of dry sediment. Units=mgP/g dry sediment
deltaP<-deltaSRPmass/drymass_g

#Bind calculated fields to dataframe
epc<-cbind(epcRAW,drymass_g,deltaSRPconc,deltaSRPmass,deltaP,Solution_Volume_L)
View(epc)

write.csv(epc,file="data/sedimentPCl/postRdatasheet.csv")

### PLOTS - INDIVIDUAL LAKES, USE TO CALCULATE EPC
ggplot()
library(tidyverse)


#1. 0
GV1<-subset(epc,Cl_Treatment== 0)
GV1_filter<- GV1%>%
  filter(deltaP < 167 | deltaP > 168)
plot(GV1_filter$deltaP~GV1_filter$Initial_SRP_mgL,pch=20,main="Green Valley Site 1 2019",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
GV1_EPC<-lm(deltaP~Initial_SRP_mgL,data=GV1_filter)
abline(GV1_EPC,col="cadetblue3",lwd=2)
GV1_EPC #y=-16.5+686.1X
# Calculate x-intercept: 0.01360992 mg/L = 13 ug/L
16.5/686.1
abline(h=0,v=0.01360992,lwd=2,col="grey40")


#3. 50
GV3<-subset(epc,Cl_Treatment== 2)
plot(GV3$deltaP~GV3$Initial_SRP_mgL,pch=20,main="Green Valley Site 3 2019",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
GV3_EPC<-lm(deltaP~Initial_SRP_mgL,data=GV3)
abline(GV3_EPC,col="cadetblue3",lwd=2)
GV3_EPC #y=-3.84 +493.48 x
# Calculate x-intercept: 0.00778147 mg/L  7 ug/L
3.84 /493.48 
abline(h=0,v=0.00778147,lwd=2,col="grey40")

#4. 100
GV4<-subset(epc,Cl_Treatment== 4)
plot(GV4$deltaP~GV4$Initial_SRP_mgL,pch=20,main="Green Valley Site 4 2019",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
GV4_EPC<-lm(deltaP~Initial_SRP_mgL,data=GV4)
abline(GV4_EPC,col="cadetblue3",lwd=2)
GV4_EPC #y= -16.13 +508.48 x
# Calculate x-intercept:0.03172199 mg/L 32 ug/L
16.13/508.48 
abline(h=0,v=0.03172199,lwd=2,col="grey40")

#5. 500
GV5<-subset(epc,Cl_Treatment== 20)
GV5_filter <- GV5%>%
  filter(deltaP <133 | deltaP > 134)
plot(GV5$deltaP~GV5$Initial_SRP_mgL,pch=20,main="500 mg/L Chloride",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
GV5_EPC<-lm(deltaP~Initial_SRP_mgL,data=GV5)
abline(GV5_EPC,col="cadetblue3",lwd=2)
GV5_EPC #y= -15.24+489.91 x
# Calculate x-intercept: 0.03110775 mg/L  31.1 ug/L
15.24/489.91
9.188/545.237
abline(h=0,v=  0.03110775,lwd=2,col="grey40")

ggsave('figures/ECP_500.png', height = 6, width = 8, dpi = 500, units = 'in')

#2. 700
GV2<-subset(epc,Cl_Treatment== 28)
plot(GV2$deltaP~GV2$Initial_SRP_mgL,pch=20,main="Green Valley Site 2 2019",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
GV2_EPC<-lm(deltaP~Initial_SRP_mgL,data=GV2)
abline(GV2_EPC,col="cadetblue3",lwd=2)
GV2_EPC #y=-23.72 +616.47  x
# Calculate x-intercept:0.03847714 mg/L  38 ug/L
23.72/616.47 
abline(h=0,v=0.03847714,lwd=2,col="grey40")


x = c(0, 50, 100, 500, 700)

y = c(13.6, 7.8, 31.7, 31.1, 38.4)

EPC_chloride = tibble(chloride = x, EPC =y)


ggplot(EPC_chloride)+
  geom_point(aes(x = chloride, y = EPC), size = 3)+
  xlab("Chloride (mg/L)")+
  ylab("Equilibrium Phosphorus Concentration (µg/L)")+
  theme_bw(base_size = 12)

ggsave('figures/ECP.png', height = 6, width = 8, dpi = 500, units = 'in')
