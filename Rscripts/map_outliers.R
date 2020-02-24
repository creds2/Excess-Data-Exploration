# Map outliers
library(sf)
library(dplyr)
library(tmap)
library(ggplot2)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
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

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))
all$SocGrade_AB <- all$SocGrade_AB * 100
all$SocGrade_C1 <- all$SocGrade_C1 * 100
all$SocGrade_C2 <- all$SocGrade_C2 * 100
all$SocGrade_DE <- all$SocGrade_DE * 100

names(all) <- gsub(".","_", names(all), fixed = TRUE)
names(all) <- gsub(" ","_", names(all), fixed = TRUE)

res_gas <- readRDS("data/importance_gas_tree.Rds")
top_gas <- rownames(res_gas)[res_gas[,1] > (max(res_gas[,1]) / 4)]
top_gas <- top_gas[top_gas != "(Intercept)"]

model_gas <- lm(as.formula(paste0("MeanDomGas_11_kWh ~ ",paste(top_gas, collapse = " + "))),
                data = all)

all_gas <- all[!is.na(all$MeanDomGas_11_kWh),]
all_gas$gas_predict <- predict(model_gas)
all_gas$gas_diff <- all_gas$gas_predict - all_gas$MeanDomGas_11_kWh
summary(all_gas$gas_diff)

all_gas = all_gas[,c("LSOA11","RUC11","gas_diff")]
all_gas = left_join(bounds, all_gas, by = c("LSOA11"))

map <- tm_shape(all_gas) +
  tm_fill(col = "gas_diff",
          palette = "-RdYlBu",
          title = "kWh per year",
          style = "fixed",
          breaks = quantile(all_gas$gas_diff, seq(0,1,0.1), na.rm = TRUE)) +
  tm_shape(urban) +
  tm_borders(col = "black")
tmap_save(map, "plots/gas_outliers2.png",
          dpi = 300)

#all_ru <- left_join(all, ru, by = "LSOA11")
all_gas_plot = all_gas[!is.na(all_gas$RUC11),]
ggplot(all_gas_plot, aes(x = stringr::str_wrap(RUC11, 15), y = gas_diff)) +
  geom_boxplot() +
  coord_flip()


res_elec <- readRDS("data/importance_elec_tree.Rds")
top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

model_elec <- lm(as.formula(paste0("MeanDomElec_11_kWh ~ ",paste(top_elec, collapse = " + "))),
                data = all)

all_elec <- all[!is.na(all$MeanDomElec_11_kWh),]
all_elec$elec_predict <- predict(model_elec)
all_elec$elec_diff <- all_elec$elec_predict - all_elec$MeanDomElec_11_kWh
summary(all_elec$elec_diff)

all_elec = all_elec[,c("LSOA11","RUC11","elec_diff")]
all_elec = left_join(bounds, all_elec, by = c("LSOA11"))

map <- tm_shape(all_elec) +
  tm_fill(col = "elec_diff",
          palette = "-RdYlBu",
          title = "kWh per year",
          style = "fixed",
          breaks = quantile(all_elec$elec_diff, seq(0,1,0.1), na.rm = TRUE)) +
  tm_shape(urban) +
  tm_borders(col = "black")
tmap_save(map, "plots/elec_outliers.png",
          dpi = 300)


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

all_miles = all_miles[,c("LSOA11","RUC11","miles_diff")]
all_miles = left_join(bounds, all_miles, by = c("LSOA11"))

map <- tm_shape(all_miles) +
  tm_fill(col = "miles_diff",
          palette = "-RdYlBu",
          title = "kWh per year",
          style = "fixed",
          breaks = quantile(all_miles$miles_diff, seq(0,1,0.1), na.rm = TRUE)) +
  tm_shape(urban) +
  tm_borders(col = "black")
tmap_save(map, "plots/miles_outliers.png",
          dpi = 300)



#all_ru <- left_join(all, ru, by = "LSOA11")
all_elec_plot = all_elec[!is.na(all_elec$RUC11),]
ggplot(all_elec_plot, aes(x = stringr::str_wrap(RUC11, 15), y = elec_diff)) +
  geom_boxplot() +
  coord_flip()