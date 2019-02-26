# Emissions

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

# Subset ----------------------------------------------------------------

lsoa <- lsoa[,c("LSOA11CD","Region","elecAv","gasAv","nrgHH","Income")]
lsoa$elecAvCO2 <- lsoa$elecAv * 0.283
lsoa$gasAvCO2 <- lsoa$gasAv *   0.204
lsoa$nrgHHCO2 <- lsoa$nrgHH *   0.246
lsoa$gasAvCO2[is.na(lsoa$gasAvCO2)] <- 0


lsoa$allCO2 <- lsoa$elecAvCO2 + lsoa$gasAvCO2 + lsoa$nrgHHCO2
summary(lsoa$allCO2)

library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")
zones <- st_read("E:/OneDrive/Data/2011 OA Boundaries/LSOA_2011_EW_BFC_shp/LSOA_2011_EW_BFC.shp")
underbudget <- left_join(zones, lsoa, by = "LSOA11CD")
underbudget <- underbudget[underbudget$allCO2 < 3500,]
qtm(underbudget, fill = "red")
nrow(underbudget) / nrow(lsoa)


lsoa$Income_decile <- as.numeric(cut(lsoa$Income, breaks = quantile(lsoa$Income, probs = seq(0, 1, 0.1)), 
    include.lowest = TRUE, labels = 1:10))

summary(lsoa$elecAv[lsoa$Income_decile == 5])
summary(lsoa$gasAv[lsoa$Income_decile == 5])
summary(lsoa$nrgHH[lsoa$Income_decile == 5])

summary(lsoa$elecAv)
summary(lsoa$gasAv)
summary(lsoa$nrgHH)
