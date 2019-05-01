# prep population
library(dplyr)

dir.create("temp")
unzip("data-input/Population/rftmid2011lsoatable.zip", exdir = "temp")
pop2011 <- readxl::read_excel("temp/mid-2011-lsoa-quinary-estimates.xls", sheet = "Mid-2011 Persons")
unlink("temp", recursive = T)

names(pop2011) <- as.character(pop2011[3,])
pop2011 <- pop2011[4:nrow(pop2011),]
pop2011 <- pop2011[,c(1,3,4)]
names(pop2011) <- c("LSOA11", "Name","pop2011")
pop2011 <- pop2011[!is.na(pop2011$Name),]

dir.create("temp")
unzip("data-input/Population/sape20dt1mid2016lsoasyoaestimatesformatted.zip", exdir = "temp")
pop2016 <- readxl::read_excel("temp/SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls", sheet = "Mid-2016 Persons")
unlink("temp", recursive = T)

names(pop2016) <- as.character(pop2016[3,])
pop2016 <- pop2016[4:nrow(pop2016),]
pop2016 <- pop2016[,c(1,3,4)]
names(pop2016) <- c("LSOA11", "Name","pop2016")
pop2016 <- pop2016[!is.na(pop2016$Name),]


pop2011 <- pop2011[,c("LSOA11","pop2011")]
pop2016 <- pop2016[,c("LSOA11","pop2016")]

pop_all <- left_join(pop2011, pop2016, by = "LSOA11")

saveRDS(pop_all,"data-prepared/population.Rds")
