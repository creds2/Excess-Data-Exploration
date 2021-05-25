# Map outliers
library(sf)
library(dplyr)
library(tmap)
library(ggplot2)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else if(dir.exists("D:/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "D:/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
}

bounds = read_sf("data-prepared/LSOA_forplots.gpkg")
bounds$country = substr(bounds$LSOA11, 1, 1)
bounds = bounds[bounds$country != "S",]

ru <- readRDS("data-prepared/ruralurban.Rds")
ru <- left_join(bounds, ru, by = "LSOA11")
urban <- ru[ru$RUC11 %in% c("Urban major conurbation","Urban city and town","Urban minor conurbation"),]
urban <- group_by(urban, RUC11) %>%
  summarise()
#urban = st_union(urban$geom)
la <- readRDS("data-prepared/la_bounds.Rds")

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all2.Rds"))



res_gas <- readRDS("data/importance_gas_tree_final.Rds")
top_gas <- rownames(res_gas)[res_gas[,1] > (max(res_gas[,1]) / 4)]
top_gas <- top_gas[top_gas != "(Intercept)"]

model_gas <- lm(as.formula(paste0("gas_kwh_percap ~ ",paste(top_gas, collapse = " + "))),
                data = all)

all_gas <- all[!is.na(all$gas_kwh_percap),]
all_gas$gas_predict <- predict(model_gas)
all_gas$gas_diff <- all_gas$gas_predict - all_gas$gas_kwh_percap
all_gas$gas_diff_per <- all_gas$gas_diff / all_gas$gas_predict * 100




summary(all_gas$gas_diff)
summary(all_gas$gas_diff_per)

all_gas = all_gas[,c("LSOA11","median_household_income","gas_diff")]
all_gas = left_join(bounds, all_gas, by = c("LSOA11"))

median_income <- median(all$median_household_income, na.rm = TRUE)

gas_rich <- all_gas[all_gas$median_household_income >= median_income,]
gas_poor <- all_gas[all_gas$median_household_income < median_income,]

gas_rich <- gas_rich[!is.na(gas_rich$gas_diff),]
gas_rich <- gas_rich[gas_rich$gas_diff > 0,]

gas_poor <- gas_poor[!is.na(gas_poor$gas_diff),]
gas_poor <- gas_poor[gas_poor$gas_diff > 0,]


map <- tm_shape(gas_rich, bbox = st_bbox(la)) +
  tm_fill(col = "gas_diff", 
          palette = "Purples",
          title = "Gas Overconsumption\n(kWh per capita)\nHigh income areas",
          breaks = c(0,1000,2000,6100)) +
  tm_legend(position = c(0,0.7)) +
  tm_shape(gas_poor) +
  tm_fill(col = "gas_diff",
          palette = "Greens",
          title = "Low income areas",
          breaks = c(0,1000,2000,6100)) +
  tm_shape(la) +
  tm_borders()

tmap_save(map,"plots/gas_overconsumers2.png")

res_elec <- readRDS("data/importance_elec_tree_final.Rds")
top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

model_elec <- lm(as.formula(paste0("elec_kwh_percap ~ ",paste(top_elec, collapse = " + "))),
                data = all)

all_elec <- all[!is.na(all$elec_kwh_percap),]
all_elec$elec_predict <- predict(model_elec)
all_elec$elec_diff <- all_elec$elec_predict - all_elec$elec_kwh_percap
summary(all_elec$elec_diff)

all_elec = all_elec[,c("LSOA11","median_household_income","elec_diff")]
all_elec = left_join(bounds, all_elec, by = c("LSOA11"))

elec_rich <- all_elec[all_elec$median_household_income >= median_income,]
elec_poor <- all_elec[all_elec$median_household_income < median_income,]

elec_rich <- elec_rich[!is.na(elec_rich$elec_diff),]
elec_rich <- elec_rich[elec_rich$elec_diff > 0,]

elec_poor <- elec_poor[!is.na(elec_poor$elec_diff),]
elec_poor <- elec_poor[elec_poor$elec_diff > 0,]


map <- tm_shape(elec_rich, bbox = st_bbox(la)) +
  tm_fill(col = "elec_diff", 
          palette = "Purples",
          title = "Electricity Overconsumption\n(kWh per capita)\nHigh income areas",
          breaks = c(0,100,500,2600)) +
  tm_legend(position = c(0,0.7)) +
  tm_shape(elec_poor) +
  tm_fill(col = "elec_diff",
          palette = "Greens",
          title = "Low income areas",
          breaks = c(0,100,500,2600)) +
  tm_shape(la) +
  tm_borders()

tmap_save(map,"plots/electric_overconsumers2.png")

res_miles <- readRDS("data/importance_miles_per_cap_tree.Rds")
top_miles <- rownames(res_miles)[res_miles[,1] > (max(res_miles[,1]) / 4)]
top_miles <- top_miles[top_miles != "(Intercept)"]
all_miles <- all[!is.na(all$miles_percap),]
all_miles <- all_miles[!is.na(all_miles$acc_town_PTfrequency),]

model_miles <- lm(as.formula(paste0("miles_percap ~ ",paste(top_miles, collapse = " + "))),
                 data = all_miles)

all_miles$miles_predict <- predict(model_miles)
all_miles$miles_diff <- all_miles$miles_predict - all_miles$miles_percap
summary(all_miles$miles_diff)

all_miles = all_miles[,c("LSOA11","median_household_income","miles_diff")]
all_miles = left_join(bounds, all_miles, by = c("LSOA11"))

all_miles$km_diff <- all_miles$miles_diff * 1.60934

miles_rich <- all_miles[all_miles$median_household_income >= median_income,]
miles_poor <- all_miles[all_miles$median_household_income < median_income,]

miles_rich <- miles_rich[!is.na(miles_rich$miles_diff),]
miles_rich <- miles_rich[miles_rich$miles_diff > 0,]

miles_poor <- miles_poor[!is.na(miles_poor$miles_diff),]
miles_poor <- miles_poor[miles_poor$miles_diff > 0,]


map <- tm_shape(miles_rich, bbox = st_bbox(la)) +
  tm_fill(col = "km_diff", 
          palette = "Purples",
          title = "Driving Overconsumption\n(km per capita)\nHigh income areas",
          breaks = c(0,500,1000,5200)) +
  tm_legend(position = c(0,0.7)) +
  tm_shape(miles_poor) +
  tm_fill(col = "km_diff",
          palette = "Greens",
          title = "Low income areas",
          breaks = c(0,500,1000,5200)) +
  tm_shape(la[!la$la %in% c("Gwynedd","Ceredigion","Powys","Pembrokeshire",
                            "Carmarthenshire","Swansea","Neath Port Talbot",
                            "Bridgend","The Vale of Glamorgan","Cardiff","Caerphilly",
                            "Newport","Monmouthshire","Merthyr Tydfil",
                            "Isle of Anglesey","Conwy","Denbighshire",
                            "Flintshire","Wrexham","Rhondda Cynon Taf",
                            "Blaenau Gwent","Torfaen"),]) +
  tm_borders()

tmap_save(map,"plots/miles_overconsumers2.png")
