# prep area classifications

lsoa <- readxl::read_xls("data-input/Area Classification/clustermembershipv2.082018.xls", sheet = "Clusters by SOA")
lsoa <- as.data.frame(lsoa)
head(lsoa)
names(lsoa) <- lsoa[11,]
lsoa <- lsoa[12:nrow(lsoa),]

saveRDS(lsoa,"data-prepared/area_classification.Rds")
