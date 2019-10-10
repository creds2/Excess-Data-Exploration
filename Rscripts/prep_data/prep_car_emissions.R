# process fuel efficnecy data
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
tmap_mode("plot")
path = "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.xlsx"

emissions = readxl::read_xlsx(path, sheet = "LSOA")
emissions = emissions[emissions$LSOA != "zBetween Keepers",]
emissions = emissions[emissions$LSOA != "zUnknown",]
emissions[3:18] = lapply(emissions[3:18], function(x){as.numeric(gsub("c","",x))})


emissions_summary1 = emissions[,c("LSOA","Fuel","Average CO2 (g/km)")]
emissions_summary2 = emissions[,c("LSOA","Fuel","Total")]
emissions_summary1 = emissions_summary1 %>%
  spread(Fuel, `Average CO2 (g/km)`)
emissions_summary2 = emissions_summary2 %>%
  spread(Fuel, Total)      
emissions_summary2$all_cars = rowSums(emissions_summary2[,2:6], na.rm = TRUE)


names(emissions_summary1) <- paste0(tolower(names(emissions_summary1)),"_co2")
names(emissions_summary2) <- paste0(tolower(names(emissions_summary2)),"_n")
names(emissions_summary1)[1] <- "LSOA"
names(emissions_summary2)[1] <- "LSOA"
emissions_summary = left_join(emissions_summary2, emissions_summary1, by = "LSOA")
# some lsoa have 200,000 cars! remove a few places
emissions_summary = emissions_summary[emissions_summary$all_cars_n < 5000, ]






saveRDS(emissions_summary, "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.Rds")




# Make some plots

bounds <- read_sf("data-prepared/LSOA_forplots.gpkg")
bounds <- left_join(bounds, emissions_summary, by = c("LSOA11" = "LSOA"))

map <- tm_shape(bounds) +
  tm_fill(col = "petrol_co2",
          breaks = c(100,120,130,140,150,160,170,180,200,250),
          palette = "-RdYlBu") +
  tm_layout(title = "Average CO2 emissions of petrol vehicles per km")

tmap_save(map, file = "plots/emissions_petrol_ave_co2.png")

map <- tm_shape(bounds) +
  tm_fill(col = "diesel_co2",
          breaks = c(100,120,130,140,150,160,170,180,200,250),
          palette = "-RdYlBu") +
  tm_layout(title = "Average CO2 emissions of diesel vehicles per km")

tmap_save(map, file = "plots/emissions_diesel_ave_co2.png")

summmary(bounds$diesel_co2)


