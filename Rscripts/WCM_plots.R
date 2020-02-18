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


gas <- all$MeanDomGas_11_kWh
gas <- gas[!is.na(gas)]
gas <- gas[order(gas)]

plot(gas, xlab = "Number of LSOAs", ylab = "Mean domestic gas (kWh)")


elec <- all$MeanDomElec_11_kWh
elec <- elec[!is.na(elec)]
elec <- elec[order(elec)]

plot(elec, xlab = "Number of LSOAs", ylab = "Mean domestic electricity (kWh)")

miles <- all$miles_percap
miles <- miles[!is.na(miles)]
miles <- miles[order(miles)]

plot(miles, xlab = "Number of LSOAs", ylab = "Miles per capita", ylim = c(0, 5e4))
