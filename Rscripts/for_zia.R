library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(tidyr)
onedrive <- "E:/OneDrive - University of Leeds/"

lad = read.csv("data-input/OA Lookup/oa_lookup_dec_2011.csv", stringsAsFactors = FALSE)
lad = lad[,c(2,7)]
lad = unique(lad)


dz = read.csv("data-input/OA Lookup/Datazone2011Lookup.csv", stringsAsFactors = FALSE)
dz = dz[,c("DataZone","Council")]
dzcode = read.csv("data-input/OA Lookup/ca2019_codes_and_labels_21042020.csv", stringsAsFactors = FALSE)
dzcode = dzcode[,c("CA","CAName")]
dz = left_join(dz, dzcode, c("Council" = "CA"))
dz = dz[,c("DataZone","CAName")]
names(dz) = names(lad)
lad = rbind(lad, dz)

emissions <- readRDS(paste0(onedrive,"Data/CREDS Data/github-secure-data/lsoa_emissions_hisorical_long.Rds" ))
emissions <- emissions[,c("year","LSOA","fuel","AllCars")]
emissions <- left_join(emissions, lad, by = c("LSOA" = "LSOA11CD"))
emissions$country <- substr(emissions$LSOA,1,1)

summ <- emissions %>%
  group_by(year, LAD11NM) %>%
  summarise(AllCars = sum(AllCars, na.rm = TRUE))


summ <- pivot_wider(summ, names_from = "year", values_from = "AllCars")
write.csv(summ, "data-output/LA_number_cars_year.csv", row.names = FALSE)

# Flag Unusual LSOA
top <- emissions %>%
  group_by(year, LSOA) %>%
  summarise(AllCars = sum(AllCars, na.rm = TRUE),
            LAD11NM = LAD11NM[1])

top <- top[order(top$AllCars,decreasing = TRUE),]
quantile(top$AllCars, seq(0.8,1,0.01))
top <- top[top$AllCars > 3000,]

write.csv(top, "data-output/high_car_LSOA_year.csv", row.names = FALSE)
