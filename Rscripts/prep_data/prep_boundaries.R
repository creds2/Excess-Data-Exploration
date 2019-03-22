# prep boundaries
library(sf)

# Super generalised

dir.create("tmp")
unzip("data-input/Boundaires/England_lsoa_2011_sgen_clipped.zip", exdir = "tmp")
england <- sf::st_read("tmp/england_lsoa_2011_sgen_clipped.shp")
england <- england[,c("code")]
unlink("tmp", recursive = T)

dir.create("tmp")
unzip("data-input/Boundaires/Wales_lsoa_2011_sgen_clipped.zip", exdir = "tmp")
wales <- sf::st_read("tmp/wales_lsoa_2011_sgen_clipped.shp")
wales <- wales[,c("code")]
unlink("tmp", recursive = T)

dir.create("tmp")
unzip("data-input/Boundaires/SG_DataZoneBdry_2011.zip", exdir = "tmp")
scotland <- sf::st_read("tmp/SG_DataZone_Bdry_2011.shp")
scotland$DataZone <- as.character(scotland$DataZone)
scotland <- scotland[,c("DataZone")]
names(scotland) <- c("LSOA11","geometry")

unlink("tmp", recursive = T)

EW_gen <- rbind(england, wales)
rm(england, wales)
names(EW_gen) <- c("LSOA11","geometry")

scotland_gen <- sf::st_simplify(scotland, dTolerance = 10)

gb_gen <- rbind(EW_gen, scotland_gen)
gb_gen <- st_cast(gb_gen, "MULTIPOLYGON")
write_sf(gb_gen,"data-prepared/LSOA_generalised.gpkg")

dir.create("tmp")
unzip("data-input/Boundaires/England_lsoa_2011_clipped.zip", exdir = "tmp")
england <- sf::st_read("tmp/england_lsoa_2011_clipped.shp")
england <- england[,c("code")]
unlink("tmp", recursive = T)

dir.create("tmp")
unzip("data-input/Boundaires/Wales_lsoa_2011_clipped.zip", exdir = "tmp")
wales <- sf::st_read("tmp/wales_lsoa_2011_clipped.shp")
wales <- wales[,c("code")]
unlink("tmp", recursive = T)

EW_full <- rbind(england, wales)
rm(england, wales)
names(EW_full) <- c("LSOA11","geometry")
gb_full <- rbind(EW_full, scotland)
gb_full <- st_cast(gb_full, "MULTIPOLYGON")
write_sf(gb_gen,"data-prepared/LSOA_full.gpkg")
