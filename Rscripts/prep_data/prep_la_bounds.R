library(sf)
library(dplyr)

dir.create("tmp")
unzip("data-input/Boundaires/England_lsoa_2011_sgen_clipped.zip", exdir = "tmp")
england <- sf::st_read("tmp/england_lsoa_2011_sgen_clipped.shp")
#england <- england[,c("code")]
unlink("tmp", recursive = T)

dir.create("tmp")
unzip("data-input/Boundaires/Wales_lsoa_2011_sgen_clipped.zip", exdir = "tmp")
wales <- sf::st_read("tmp/wales_lsoa_2011_sgen_clipped.shp")
#wales <- wales[,c("code")]
unlink("tmp", recursive = T)


bounds <- rbind(england, wales)


la <- strsplit(as.character(bounds$name)," ")
la <- sapply(la, function(x){paste(x[seq_len(length(x) - 1)], collapse = " ")})

bounds$la <- la

bounds <- group_by(bounds, la) %>%
  summarise()

saveRDS(bounds, "data-prepared/la_bounds.Rds")
