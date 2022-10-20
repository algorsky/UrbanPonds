### working with 2018 EPC pilot experiment results with the goal of refining calculations for 2019

getwd()
setwd("/Users/adriannagorsky/Documents/UrbanPonds/data/sedimentPCl/Wingra_Shallow")

epcRAW<-read.csv("EPC_22_clean_WS.csv")
View(epcRAW)

# Calculations
  #1. Dry mass equivalent of fresh sediment used (used average moisture content for 0-4cm). Units=g
drymass_g<-epcRAW$Mass_sediment_fresh_g*(1-epcRAW$MC_frac)
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

write.csv(epc,file="postRdatasheet.csv")

### PLOTS - INDIVIDUAL LAKES, USE TO CALCULATE EPC
#1. Chloride 0
Cl_0<-subset(epc,Cl_Treatment=="0")
plot(Cl_0$deltaP~Cl_0$Initial_SRP_mgL,pch=20,main="Chloride 0 mg/L",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
Cl_0_EPC<-lm(deltaP~Initial_SRP_mgL,data=Cl_0)
abline(Cl_0_EPC,col="cadetblue3",lwd=2)
Cl_0_EPC #y=-5.935+399.145X
# Calculate x-intercept:  0.01486928 mg/L
5.935/399.145  
abline(h=0,v= 0.01486928,lwd=2,col="grey40")

#2. Chloride 50
Cl_50<-subset(epc,Cl_Treatment=="50")
plot(Cl_50$deltaP~Cl_50$Initial_SRP_mgL,pch=20,main="Chloride 50 mg/L",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
Cl_50_EPC<-lm(deltaP~Initial_SRP_mgL,data=Cl_50)
abline(Cl_50_EPC,col="cadetblue3",lwd=2)
Cl_50_EPC #y=-9.598 +371.240 x
# Calculate x-intercept: 0.0258539 mg/L
9.598 /371.240 
abline(h=0,v=0.0258539,lwd=2,col="grey40")

#3. Chloride 100
Cl_100<-subset(epc,Cl_Treatment=="100")
plot(Cl_100$deltaP~Cl_100$Initial_SRP_mgL,pch=20,main="Chloride 100 mg/L",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
Cl_100_EPC<-lm(deltaP~Initial_SRP_mgL,data=Cl_100)
abline(Cl_100_EPC,col="cadetblue3",lwd=2)
Cl_100_EPC #y= -5.623   + 226.316 x
# Calculate x-intercept: 0.02484579 mg/L
5.623  / 226.316
abline(h=0,v=0.02484579,lwd=2,col="grey40")

#4. Chloride 500
Cl_500<-subset(epc,Cl_Treatment=="500")
plot(Cl_500$deltaP~Cl_500$Initial_SRP_mgL,pch=20,main="Chloride 500 mg/L",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
Cl_500_EPC<-lm(deltaP~Initial_SRP_mgL,data=Cl_500)
abline(Cl_500_EPC,col="cadetblue3",lwd=2)
Cl_500_EPC #y=-0.7774  +4.5036x
# Calculate x-intercept: 0.1726175 mg/L
0.7774/4.5036
abline(h=0,v=0.1726175,lwd=2,col="grey40")

#5. Chloride 1000
Cl_1000<-subset(epc,Cl_Treatment=="1000")
plot(Cl_1000$deltaP~Cl_1000$Initial_SRP_mgL,pch=20,main="Chloride 1000mg/L",xlab="Initial Aqueous-Phase P (SRP, mg/L)", ylab="Sorbed Phosphorus (mgP/g dry sediment)")
Cl_1000_EPC<-lm(deltaP~Initial_SRP_mgL,data=Cl_1000)
abline(Cl_1000_EPC,col="cadetblue3",lwd=2)
Cl_1000_EPC #y=-0.8054+ 4.2473  x
# Calculate x-intercept:  0.1896264 mg/L
0.8054  / 4.2473  
abline(h=0,v=0.1753302,lwd=2,col="grey40")








