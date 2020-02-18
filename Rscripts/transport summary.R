# Transport Summary
library(tmap)

mot <- readRDS("../Secure-Data/Excess/mot_2011.Rds")
bounds <- sf::read_sf("data-prepared/LSOA_forplots.gpkg")
bounds <- dplyr::left_join(bounds, mot, by = c("LSOA11" = "LSOA"))
#bounds$pchange = (bounds$`2017` -  bounds$`2013`) / bounds$`2013` * 100

map <- tm_shape(bounds) +  # Build the map
  tm_fill("miles_percap",
          breaks = quantile(bounds$miles_percap, probs = seq(0,1,0.1), na.rm = TRUE),
          palette = "-RdYlBu", 
          legend.title = "Car Miles per capita 2011")
tmap_save(map, filename = "plots/energy_trends/car_miles.png", dpi = 600)

tm_shape(bounds[1234,]) +
  tm_fill(col = "red", alpha = 0.5)
