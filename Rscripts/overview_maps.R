# Briefing Note Plots

# Librarys

library(tmap)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
tmap_mode("plot")

# Import Data  -----------------------------------------------
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/"
}

la <- readRDS("data-prepared/la_bounds.Rds")

bounds <- st_read("data-prepared/LSOA_generalised.gpkg")

all = readRDS(paste0(secure_path,"Data/CREDS Data/github-secure-data/lsoa_all.Rds"))

all = all[,c("MeanDomElec_11_kWh","MeanDomGas_11_kWh","driving_kwh_percap")]



# Make a map
all$total_percap <- all$driving_kwh_percap + all$gas_percap + all$elec_percap + all$flight_kwh_percap + all$nongas_percap

all_sf <- left_join(bounds, all)
all_sf <- all_sf[!is.na(all_sf$income_quantile),]

tm <- tm_shape(all_sf) +
  tm_fill("total_percap", 
          style = "fixed", 
          palette = "-RdYlBu",
          midpoint = median(all_sf$total_percap),
          breaks = quantile(all_sf$total_percap, probs = seq(0,1,0.1)),
          title = "Selected Annual Energy Use (kWh per capita)") +
  tm_shape(la) +
    tm_borders()

tmap_save(tm, filename = "plots/briefing_note_map.png")
          #breaks = quantile(all_sf$total_percap, probs = seq(0,1,0.1)))

tm2 <- tm_shape(all_sf) +
  tm_fill("total_percap", 
          style = "fixed", 
          palette = "-RdYlBu",
          midpoint = median(all_sf$total_percap),
          breaks = c(1000,5000,8000,10000,12000,14000,16000,18000,20000,30000,41000),
          title = "Selected Annual Energy Use (kWh per capita)",
          auto.palette.mapping=FALSE) +
  tm_shape(la) +
  tm_borders()

tmap_save(tm2, filename = "plots/briefing_note_map_alt.png")
