library(sf)
library(dplyr)
library(lwgeom)

bounds <- st_read("data-prepared/LSOA_generalised.gpkg")
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

st_crs(bounds) <- 27700
bounds <- st_transform(bounds, 4326)

bounds <- left_join(bounds,lsoa, by = c("LSOA11" = "LSOA11CD"))

#reduce the presision
bounds$geom <- st_as_binary(bounds$geom, precision = 10000)
bounds$geom <- st_as_sfc(bounds$geom)

#validate the geometries
bounds <- st_make_valid(bounds)
#saveRDS(bounds,"E:/OneDrive - University of Leeds/wheretolive/data/creds.Rds")

#convert to well known text
bounds$geom <- st_as_text(bounds$geom)
bounds <- as.data.frame(bounds)

bounds$id <- 1:nrow(bounds)
bounds <- bounds[,c("id",names(bounds)[!names(bounds) %in% "id"])]

#fix not allowed names
names(bounds)[names(bounds) == "cluster"] <- "Ccluster"
names(bounds)[names(bounds) == "Private"] <- "CPrivate"
names(bounds)[names(bounds) == "Old"] <- "COld"
names(bounds)[names(bounds) == "all"] <- "CCall"


write.csv(bounds, "E:/OneDrive - University of Leeds/wheretolive/data/creds.csv", row.names = FALSE, na = "")


