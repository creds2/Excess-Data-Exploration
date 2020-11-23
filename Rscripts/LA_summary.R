library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
# heatline results
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
}

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all2.Rds"))

lad = read.csv("data-input/OA Lookup/oa_lookup_dec_2011.csv", stringsAsFactors = FALSE)
lad = lad[,c(2,7)]
lad = unique(lad)

sub = all[,c("LSOA11","TotDomElec_11_kWh","TotDomGas_11_kWh","cars_total",
             "cars_miles","vans_total","vans_miles","petrol_emissions","diesel_emissions")]

sub <- left_join(sub, lad, by = c("LSOA11" = "LSOA11CD"))

fin <- sub %>%
  select(!c(vans_total, vans_miles, LSOA11)) %>%
  group_by(LAD11NM) %>%
  summarise_all(sum, na.rm = TRUE)

write.csv(fin, "data-output/LA_summary.csv", row.names = FALSE)

emissions <- readr::read_csv("E:/OneDrive - University of Leeds/Share/Historical_Car_Emissions_LSOA_public.zip")
emissions <- left_join(emissions, lad, by = c("LSOA" = "LSOA11CD"))
emissions$AllCars[is.na(emissions$AllCars)] <- 1

emissions2 <-  emissions %>%
  group_by(LAD11NM, year, fuel) %>%
  summarise(AllCars = sum(AllCars, na.rm = TRUE),
            AvgCO2 = mean(AvgCO2, na.rm = TRUE),
            AvgAge = mean(AvgAge, na.rm = TRUE))

write.csv(emissions2, "data-output/LA_summary_emissions.csv", row.names = FALSE)
