# Map outliers
library(sf)
library(dplyr)
library(tmap)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}

bounds = read_sf("data-prepared/LSOA_forplots.gpkg")
bounds$country = substr(bounds$LSOA11, 1, 1)
bounds = bounds[bounds$country != "S",]

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

all_gas = all_gas[,c("LSOA11","gas_diff")]
all_gas = left_join(bounds, all_gas, by = c("LSOA11"))

map <- tm_shape(all_gas) +
  tm_fill(col = "gas_diff",
          palette = "-RdYlBu",
          title = "kWh per year",
          style = "fixed",
          breaks = quantile(all_gas$gas_diff, seq(0,1,0.2)))
tmap_save(map, "plots/gas_outliers2.png",
          dpi = 600)
