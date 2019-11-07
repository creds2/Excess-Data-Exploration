# bounds
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

bounds <- read_sf("data-prepared/LSOA_generalised.gpkg")
dir.create("tmp")
unzip("data-input/Local_Authority_Districts_April_2019_Boundaries_UK_BFC.zip", exdir = "tmp")
la <- read_sf("tmp/Local_Authority_Districts_April_2019_Boundaries_UK_BFC.shp")
unlink("tmp",recursive = TRUE)

la <- la[la$lad19nm %in% c("Leeds","York","Middlesbrough","Richmondshire",
                           "Harrogate","Hambleton","Scarborough","Redcar and Cleveland",
                           "Craven","Ryedale","Selby","Stockton-on-Tees"),]

la <- st_transform(la, 27700)
bounds <- st_transform(bounds, 27700)

bounds <- bounds[la,]
qtm(bounds)

all = readRDS("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_all.Rds")

bounds = left_join(bounds, all, by = c("LSOA11"))
bounds$driving_emissions <- bounds$diesel_emissions + bounds$diesel_emissions

tm_shape(bounds) +
  tm_fill(col = "driving_emissions",
          palette = "-RdYlBu") +
  tm_borders()


leeds <- la[la$lad19nm %in% c("Leeds"),]

tm_shape(bounds[leeds,]) +
  tm_fill(col = "driving_emissions",
          palette = "-RdYlBu") +
  tm_borders()


all_greg <- all[,!names(all) %in% c("diesel_n","electric diesel_n","hybrid electric_n","other_n",
                                   "petrol_n","all_cars_n","diesel_co2","electric diesel_co2","hybrid electric_co2",
                                   "other_co2","petrol_co2","petrol_emissions","diesel_emissions","driving_kwh","driving_kwh_percap")]

lsoa_north <- readxl::read_excel("data-input/lsoa north.xlsx")
lsoa_north <- left_join(lsoa_north, all_greg, by = c("LSOA11CD" = "LSOA11"))
write.csv(lsoa_north,"data/data_for_greg.csv", row.names = FALSE)
