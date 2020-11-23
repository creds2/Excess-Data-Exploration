library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
# heatline results
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
}

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all2.Rds"))

summ <- data.frame(id = names(all))
summ <- as.data.frame(summ)

