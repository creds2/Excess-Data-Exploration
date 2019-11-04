# Estimate energy use breakdown.
# goal: pichear of energy use for each lsoa by type

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}


all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))

# step 1: non gas electric heating
summary(all$pHeating_Electric)
summary(all$pHeating_Gas)
summary(all$pHeating_None)
summary(all$pHeating_Other)

# none is no extra heatin use
# other assume same avereage enrgy consumption as gas

all$TotDomOther_11_kwh <- median(all$MeanDomGas_11_kWh, na.rm = TRUE) * (all$pHeating_Other / 100) * all$Dwellings





summary(all$TotDomOther_11_kwh)
summary(all$TotDomGas_11_kWh)
tot_sum <- c(sum(all$TotDomGas_11_kWh, na.rm = TRUE), 
             sum(all$TotDomElec_11_kWh, na.rm = TRUE), 
             sum(all$TotDomOther_11_kwh),
             sum(all$driving_kwh, na.rm = TRUE))
names(tot_sum) <- c("Gas","Electric","Other Heating", "Driving")
barplot(tot_sum)
