# prep population density
library(dplyr)
library(sf)


# Read in Data
dir.create("temp")
unzip("data-input/Population/rftmid2011soadensitytables.zip", exdir = "temp")
popEW2011 <- readxl::read_excel("temp/mid-2011-lsoa-pop-density.xls", sheet = "Mid-2011 Density")
unlink("temp", recursive = T)

popS2011 <- read.csv("data-input/Population/2011-sape-t1a-corrected.csv", stringsAsFactors = FALSE)
popS2011 <- popS2011[,c(1,3)]
popS2011 <- popS2011[6:nrow(popS2011),]
names(popS2011) <- c("data-zone","pop2011")

dir.create("temp")
unzip("data-input/Population/sape20dt11mid2017lsoapopulationdensity.zip", exdir = "temp")
popEW2017 <- readxl::read_excel("temp/SAPE20DT11-mid-2017-lsoa-population-density.xls", sheet = "Mid-2017 Population Density")
popEW2017 <- as.data.frame(popEW2017)
unlink("temp", recursive = T)
names(popEW2017) <- popEW2017[4,]
popEW2017 <- popEW2017[5:nrow(popEW2017),]

popS2017 <- read.csv("data-input/Population/sape-17-persons.csv", stringsAsFactors = FALSE)
popS2017 <- popS2017[,c(1,4)]
popS2017 <- popS2017[6:(nrow(popS2017)-2),]
names(popS2017) <- c("data-zone","pop2017")

#Fromat EW
popEW2011 <- popEW2011[,c(1,3,4)]
names(popEW2011) <- c("LSOA11","area_km","dense_2011")
popEW2017 <- popEW2017[,c(1,5)]
names(popEW2017) <- c("LSOA11","dense_2017")

popEW <- left_join(popEW2011, popEW2017)

bounds <- st_read("data-prepared/LSOA_full.gpkg")
bounds_s <- bounds[bounds$LSOA11 %in% unique(c(popS2011$`data-zone`, popS2017$`data-zone`)),]

popS <- left_join(popS2017, popS2011)
popS$pop2017 <- as.numeric(gsub(",","",popS$pop2017))
popS$pop2011 <- as.numeric(gsub(",","",popS$pop2011))

popS <- left_join(popS, bounds_s, by = c("data-zone" = "LSOA11"))
popS <- st_as_sf(popS, crs = 27700)
popS$area_km <- as.numeric(st_area(popS)) / 1e6
popS$dense_2011 <- popS$pop2011 / popS$area_km
popS$dense_2017 <- popS$pop2017 / popS$area_km
popS <- st_drop_geometry(popS)

popS <- popS[,c("data-zone","area_km","dense_2011", "dense_2017")]
names(popS) <- c("LSOA11","area_km","dense_2011", "dense_2017")

popAll <- rbind(popEW, popS)
saveRDS(popAll, "data-prepared/populationDensity.Rds")
