# Transport Summary
library(tmap)

mot <- readRDS("../Secure-Data/Excess/mot_2011.Rds")
bounds <- sf::read_sf("data-prepared/LSOA_generalised.gpkg")
bounds <- dplyr::left_join(bounds, mot, by = c("LSOA11" = "LSOA"))
#bounds$pchange = (bounds$`2017` -  bounds$`2013`) / bounds$`2013` * 100

map <- tm_shape(bounds) +  # Build the map
  tm_fill("miles_percap",
          breaks = c(200,2000,4000,5000,6000,7000,8000,10000,Inf),
          palette = "-RdYlBu", 
          legend.title = "Car Miles per capita 2011")
tmap_save(map, filename = "plots/energy_trends/car_miles.png", dpi = 600)
