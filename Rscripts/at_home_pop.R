# Librarys

library(tree)
library(dplyr)
library(ggplot2)

# Import Data  -----------------------------------------------
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))
all$median_household_income[is.na(all$median_household_income)] <- median(all$median_household_income, na.rm = TRUE)

names(all) <- gsub(".","_", names(all), fixed = TRUE)
names(all) <- gsub(" ","_", names(all), fixed = TRUE)

# at home becuase: retired, sick, young, unempolyed, working from home

all$at_home <- all$Retired + all$Age0to4 + all$Sick + all$Unemployed + all$SelfEmp

# build model on main variaibles
all_gas <- all[!is.na(all$MeanDomGas_11_kWh),]

lm_predict <- lm(all_gas$MeanDomGas_11_kWh ~ all_gas$mean_rooms + 
                   all_gas$Crr_EE + all_gas$mean_household_size + 
                   all_gas$SocGrade_AB + all_gas$Outright)
all_gas$gas_predict <- predict(lm_predict)
all_gas$pred_diff <- all_gas$gas_predict - all_gas$MeanDomGas_11_kWh

plot(all_gas$at_home, all_gas$MeanDomGas_11_kWh)
plot(all_gas$at_home, all_gas$pred_diff)
cor(all_gas$at_home, all_gas$pred_diff)
cor(all_gas$Unemployed, all_gas$MeanDomGas_11_kWh)
cor(all_gas$SelfEmp, all_gas$median_household_income)
lm0 <- lm(all$MeanDomGas_11_kWh ~ all$Retired + all$Age0to4 + all$Sick + all$Unemployed + all$SelfEmp + all$mean_rooms)
summary(lm0)
