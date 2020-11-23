# process fuel efficnecy data
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(lubridate)
tmap_mode("plot")
onedrive <- "E:/OneDrive - University of Leeds/"
#path = "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.xlsx"
#path = "E:/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.xlsx"

emissions <- readr::read_csv(paste0(onedrive,"Data/CREDS Data/github-secure-data/19029 Robin Lovelace LSOA.txt" ))
names(emissions) <- c("year","LSOA","fuel","bA","bB", "bC","bD", "bE",
                     "bF", "bG", "bH", "bI",
                     "bJ", "bK", "bL", "bM",
                     "Unknown","AllCars","AvgCO2","AvgAge")

# rebase average ages
emissions$rebase <- interval(lubridate::ymd("2019-03-31"), lubridate::ymd(paste0(emissions$year,"-12-31"))) / years(1)
emissions$AvgAge <- emissions$AvgAge + emissions$rebase
emissions$rebase <- NULL
  
saveRDS(emissions, paste0(onedrive,"Data/CREDS Data/github-secure-data/lsoa_emissions_hisorical_long.Rds" ))

#write.csv(emissions, "E:/OneDrive - University of Leeds/Share/Historical_Car_Emissions_LSOA.csv", row.names = FALSE)
head(emissions)


emissions$fuel[emissions$fuel == "DIESEL"] <- "DI"
emissions$fuel[emissions$fuel == "PETROL"] <- "PT"
emissions$fuel[emissions$fuel == "HYBRID ELECTRIC"] <- "HE"
emissions$fuel[emissions$fuel == "OTHER"] <- "OT"
emissions$fuel[emissions$fuel == "ELECTRIC DIESEL"] <- "ED"






emissions2 <- emissions %>%
  pivot_wider(names_from = fuel, values_from = c("bA","bB","bC","bD","bE","bF","bG","bH","bI","bJ","bK","bL","bM","Unknown","AllCars","AvgCO2","AvgAge"))

emissions2[3:77] <- lapply(emissions2[3:77], function(x){
  x[is.na(x)] <- 0
  return(x)
})

emissions3 <- emissions2 %>%
  pivot_wider(names_from = year, values_from = names(emissions2)[!names(emissions2) %in% c("year","LSOA")] )
summary(duplicated(emissions3$LSOA))

saveRDS(emissions3, paste0(onedrive,"Data/CREDS Data/github-secure-data/lsoa_emissions_historical_wide.Rds"))

emissions_public <- emissions[,c("year","LSOA","fuel","AllCars","AvgCO2","AvgAge")]
summary(emissions_public$AllCars)
emissions_public$AllCars[emissions_public$AllCars <= 3] <- NA
summary(emissions_public$AllCars)
write.csv(emissions_public, "E:/OneDrive - University of Leeds/Share/Historical_Car_Emissions_LSOA_public.csv", row.names = FALSE)
# Make some plots

bounds <- read_sf("../Excess-Data-Exploration/data-prepared/LSOA_forplots.gpkg")
keep <- names(emissions3)
keep <- c("LSOA",keep[grep("2018",keep)],keep[grep("2008",keep)])

bounds <- left_join(bounds, emissions3[,keep], by = c("LSOA11" = "LSOA"))
bounds$KLM_2008 <- bounds$bK_DI_2008 + bounds$bK_PT_2008 + bounds$bK_HE_2008 + bounds$bK_OT_2008 + bounds$bK_ED_2008 +
  bounds$bL_DI_2008 + bounds$bL_PT_2008 + bounds$bL_HE_2008 + bounds$bL_OT_2008 + bounds$bL_ED_2008 +
  bounds$bM_DI_2008 + bounds$bM_PT_2008 + bounds$bM_HE_2008 + bounds$bM_OT_2008 + bounds$bM_ED_2008


bounds$KLM_2018 <- bounds$bK_DI_2018 + bounds$bK_PT_2018 + bounds$bK_HE_2018 + bounds$bK_OT_2018 + bounds$bK_ED_2018 +
  bounds$bL_DI_2018 + bounds$bL_PT_2018 + bounds$bL_HE_2018 + bounds$bL_OT_2018 + bounds$bL_ED_2018 +
  bounds$bM_DI_2018 + bounds$bM_PT_2018 + bounds$bM_HE_2018 + bounds$bM_OT_2018 + bounds$bM_ED_2018

bounds$All_2008 <- bounds$AllCars_DI_2008 + bounds$AllCars_PT_2008 + bounds$AllCars_HE_2008 + bounds$AllCars_OT_2008 + bounds$AllCars_ED_2008
bounds$All_2018 <- bounds$AllCars_DI_2018 + bounds$AllCars_PT_2018 + bounds$AllCars_HE_2018 + bounds$AllCars_OT_2018 + bounds$AllCars_ED_2018

bounds$pKLM_2008 <- bounds$KLM_2008 / bounds$All_2008 * 100
bounds$pKLM_2018 <- bounds$KLM_2018 / bounds$All_2018 * 100

map <- tm_shape(bounds) +
  tm_fill(col = "pKLM_2008",
          breaks = c(0,3,6,9,12,14,16,18,20,100),
          palette = "-RdYlBu") +
  tm_layout(title = "% of Cars in the K/L/M tax bands in 2008")

tmap_save(map, file = "../Excess-Data-Exploration/plots/KLM_cars_2008.png")

map <- tm_shape(bounds) +
  tm_fill(col = "pKLM_2018",
          breaks = c(0,3,6,9,12,14,16,18,20,100),
          palette = "-RdYlBu") +
  tm_layout(title = "% of Cars in the K/L/M tax bands in 2018")

tmap_save(map, file = "../Excess-Data-Exploration/plots/KLM_cars_2018.png")

quantile(bounds$pKLM_2008, probs = seq(0,1,0.1))
quantile(bounds$pKLM_2018, probs = seq(0,1,0.1), na.rm = TRUE)


