# maps for Kate Pangbourne
library(sf)
library(tmap)
tmap_mode("view")

emissions3 <- readRDS("E:/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions_historical_wide.Rds")

bounds <- read_sf("../Excess-Data-Exploration/data-prepared/LSOA_forplots.gpkg")
keep <- names(emissions3)
keep <- c("LSOA",keep[grep("2018",keep)])

bounds <- left_join(bounds, emissions3[,keep], by = c("LSOA11" = "LSOA"))

buff <- st_sfc(st_point(c(-0.457056, 51.371281)))
st_crs(buff) <- 4326
buff <- st_transform(buff, 27700)
buff <- st_buffer(buff, 10000)

qtm(buff)

bounds <- bounds[buff,]
qtm(bounds)

bounds <- st_drop_geometry(bounds)
write.csv(bounds,"data-output/cars_in_weybridge.csv")
