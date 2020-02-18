# travel to school census
library(sf)

lsoa <- readRDS("data-input/Travel to School/z_all.Rds")
lsoa <- st_as_sf(lsoa)
lsoa <- st_drop_geometry(lsoa)
lsoa <- lsoa[,c("geo_code","all","bicycle","foot","car")]

lsoa$all[is.na(lsoa$all)] <- 1
lsoa$bicycle[is.na(lsoa$bicycle)] <- 1
lsoa$foot[is.na(lsoa$foot)] <- 1
lsoa$car[is.na(lsoa$car)] <- 1

lsoa$T2S_bicycle <- lsoa$bicycle / lsoa$all * 100
lsoa$T2S_foot <- lsoa$foot / lsoa$all * 100
lsoa$T2S_car <- lsoa$car / lsoa$all * 100

lsoa <- lsoa[,c("geo_code","all","T2S_bicycle","T2S_foot","T2S_car")]
names(lsoa) <- c("LSOA11","T2S_all","T2S_bicycle","T2S_foot","T2S_car")
saveRDS(lsoa, "data-prepared/Trave2School.Rds")