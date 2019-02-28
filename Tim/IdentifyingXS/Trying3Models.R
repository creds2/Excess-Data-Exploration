# Aim: To FUrther explore Malcolms Model of energy use

# Packages and Setup ------------------------------------------------------
library(readr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(xgboost)
library(data.table)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("X:/Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa<-read.csv("temp/XSExpData1.csv")
lsoa2 <- fread("temp/XSExpData1.csv")
unlink("temp", recursive = T)


# Create Energy files---------------------------

#Do gas and electric for non-off gas-grid areas
#have taken RU out
myLetters <- letters[1:26]
#match("a", myLetters)
lsoa2[,RUnum:=match(RU,myLetters)]

elec<-lsoa2[gasAv>0, .(elecAv,SupergroupCode, Income, fp10, fpLIHC, 
                      FullTime, PartTime, SelfEmployed, Student,Unemployed, Retired, Carers, Sick, Old, Young,
                      Age_Mean, Age_Mean, AB, C1, C2, DE, Single,NoKids,
                      Outright, Mortgage,Social, Private, Detached, SemiDetached, Terraced, Flat,
                      HHsize, Rooms, NoCH )]

gas<-lsoa2[gasAv>0, .(gasAv, RU,SupergroupCode, Income, fp10, fpLIHC, 
                      FullTime, PartTime, SelfEmployed, Student,Unemployed, Retired, Carers, Sick, Old, Young,
                      Age_Mean, Age_Mean, AB, C1, C2, DE, Single,NoKids,
                      Outright, Mortgage,Social, Private, Detached, SemiDetached, Terraced, Flat,
                      HHsize, Rooms, NoCH )]

#Do car for all areas
car<-lsoa2[, .(nrgHH, RU,SupergroupCode, Income, 
                     FullTime, PartTime, SelfEmployed, Student,Unemployed, Retired, Carers, Sick, Old, Young,
                     Age_Mean, Age_Mean, AB, C1, C2, DE, Single,NoKids,
                     ptte, carte,ptttc, carttc, denonbc,denoffbc,denoffrmt,motor, pt, active, home, AvT2W)]




# Try some models ---------------------------------------------------------
lsoa_model <- lsoa[,!names(lsoa) %in% c("LSOA11CD","dom","SGN","SupergroupCode","SupergroupName","Region","Reg","cluster","cluster2","RU","RUC11CD")]
lsoa_model$gasAv[is.na(lsoa_model$gasAv)] <- 0
lsoa_model[is.na(gasAv),gasAv:=0]
lsoa_model$all <- lsoa_model$gasAv + lsoa_model$elecAv + lsoa_model$nrgHH

lsoa_sample <- lsoa_model[sample(1:nrow(lsoa_model), round(nrow(lsoa_model)/10)),]
lsoa_sample_mat <- as.matrix(lsoa_sample[,!colnames(lsoa_sample) %in% c("nrgHH","gasAv","elecAv","all")])
lsoa_model_mat <- as.matrix(lsoa_model[,!colnames(lsoa_model) %in% c("nrgHH","gasAv","elecAv","all")])


elec_mat<-as.matrix(lsoa_model[,colnames(lsoa_model) %in% c("SupergroupCode","Income","fp10","fpLIHC",
"FullTime","PartTime","SelfEmployed","Student","Unemployed","Retired","Carers","Sick","Old","Young",
"Age_Mean","Age_Mean","AB","C1","C2","DE","Single","NoKids",
"Outright","Mortgage","Social","Private","Detached","SemiDetached","Terraced","Flat",
"HHsize","Rooms","NoCH")])

gas_mat<-as.matrix(lsoa_model[,colnames(lsoa_model)%in%c("SupergroupCode","Income","fp10","fpLIHC",
"FullTime","PartTime","SelfEmployed","Student","Unemployed","Retired","Carers","Sick","Old","Young",
"Age_Mean","Age_Mean","AB","C1","C2","DE","Single","NoKids",
"Outright","Mortgage","Social","Private","Detached","SemiDetached","Terraced","Flat",
"HHsize","Rooms","NoCH")])

car_mat<-as.matrix(lsoa_model[,colnames(lsoa_model) %in% c("SupergroupCode","Income",
"FullTime","PartTime","SelfEmployed","Student","Unemployed","Retired","Carers","Sick","Old","Young",
"Age_Mean","Age_Mean","AB","C1","C2","DE","Single","NoKids",
"ptte","carte","ptttc","carttc","denonbc","denoffbc","denoffrmt","motor","pt","active","home","AvT2W")])




#elecMAT<-as.matrix(elec[,!colnames(elec) %in% c("elecAv")])
#gasMAT<-as.matrix(gas[,!colnames(gas)%in% c("gasAv")])
#carMat<-as.matrix(car[,!colnames(car) %in% c("nrgHH")])

# Electricity
#m_elec = xgboost(data = lsoa_sample_mat, label = lsoa_sample$elecAv, nrounds = 10, max_depth = 5)
m_elec = xgboost(data = elec_mat, label = lsoa_model$elecAv, nrounds = 10, max_depth = 5)



#plot(lsoa_model$elecAv, predict(m_elec, lsoa_model_mat)) +   abline(0,1,col = "red")
plot(lsoa_model$elecAv, predict(m_elec, elec_mat)) +   abline(0,1,col = "red")

importance_m_elec = xgb.importance(model = m_elec, feature_names = colnames(elec_mat))
png(filename="plots/importance_electricity.png")
xgb.plot.importance(importance_m_elec, top_n = 15)
dev.off()

# Gas
m_gas = xgboost(data = gas_mat,
                label = lsoa_model$gasAv, nrounds = 10, max_depth = 5)

plot(lsoa_model$gasAv, predict(m_gas, gas_mat)) +
  abline(0,1,col = "red")

importance_m_gas = xgb.importance(model = m_gas, feature_names = colnames(gas_mat))
png(filename="plots/importance_gas.png")
xgb.plot.importance(importance_m_gas, top_n = 15)
dev.off()

# Vehicles
m_car = xgboost(data = car_mat,
                     label = lsoa_model$nrgHH, nrounds = 10, max_depth = 5)

plot(lsoa_model$nrgHH, predict(m_car, car_mat)) +
  abline(0,1,col = "red")

importance_m_vehicles = xgb.importance(model = m_car, feature_names = colnames(car_mat))
png(filename="plots/importance_vehicles.png")
xgb.plot.importance(importance_m_vehicles, top_n = 15)
dev.off()




