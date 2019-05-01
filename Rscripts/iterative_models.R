# Itnerative models

# Group househodl by number of rooms and investigate gas and electric use

# Packages and Setup ------------------------------------------------------
library(readr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(xgboost)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

# Set up data
lsoa_model <- lsoa[,!names(lsoa) %in% c("LSOA11CD","dom","SGN","SupergroupCode","SupergroupName","Region","Reg","cluster","cluster2","RU","RUC11CD")]
lsoa_model$gasAv[is.na(lsoa_model$gasAv)] <- 0
lsoa_model$all <- lsoa_model$gasAv + lsoa_model$elecAv + lsoa_model$nrgHH
lsoa_model$Rooms <- round(lsoa_model$Rooms)

for(i in unique(lsoa_model$Rooms)){
  par(mar = rep(0.0001, 4))
  message(paste0("Doing ",i," rooms"))
  lsoa_rooms <- lsoa_model[lsoa_model$Rooms == i,]
  lsoa_sample <- lsoa_rooms[sample(1:nrow(lsoa_rooms), round(nrow(lsoa_rooms)/10)),]
  lsoa_sample_mat <- as.matrix(lsoa_sample[,!colnames(lsoa_sample) %in% c("nrgHH","gasAv","elecAv","all")])
  lsoa_model_mat <- as.matrix(lsoa_rooms[,!colnames(lsoa_rooms) %in% c("nrgHH","gasAv","elecAv","all")])
  
  # Electricity
  m_elec = xgboost(data = lsoa_sample_mat,
                   label = lsoa_sample$elecAv, nrounds = 10, max_depth = 5)
  
  # plot(lsoa_rooms$elecAv, predict(m_elec, lsoa_model_mat)) +
  #   abline(0,1,col = "red")
  
  
  importance_m_elec = xgb.importance(model = m_elec, feature_names = colnames(lsoa_model_mat))
  png(filename=paste0("plots/iterative/importance_electricity_",i,"_rooms.png"))
  xgb.plot.importance(importance_m_elec, top_n = 15)
  dev.off()
  
  # Gas
  m_gas = xgboost(data = lsoa_sample_mat,
                  label = lsoa_sample$gasAv, nrounds = 10, max_depth = 5)
  
  # plot(lsoa_rooms$gasAv, predict(m_gas, lsoa_model_mat)) +
  #   abline(0,1,col = "red")
  
  importance_m_gas = xgb.importance(model = m_gas, feature_names = colnames(lsoa_model_mat))
  png(filename=paste0("plots/iterative/importance_gas_",i,"_rooms.png"))
  xgb.plot.importance(importance_m_gas, top_n = 15)
  dev.off()
  
}





