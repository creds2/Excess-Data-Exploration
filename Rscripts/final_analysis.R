# Final Version of the Analysis 

# Libraries and Setup
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else if(dir.exists("E:/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "D:/OneDrive - University of Leeds/Data/CREDS Data"
}

# Read In Data
all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))
all$SocGrade_AB <- all$SocGrade_AB * 100
all$SocGrade_C1 <- all$SocGrade_C1 * 100
all$SocGrade_C2 <- all$SocGrade_C2 * 100
all$SocGrade_DE <- all$SocGrade_DE * 100

names(all) <- gsub(".","_", names(all), fixed = TRUE)
names(all) <- gsub(" ","_", names(all), fixed = TRUE)

classf <- readRDS("data-prepared/area_classification.Rds")
classf <- classf[,c("SOA Code","Supergroup Name","Group Code","Group Name")]
names(classf) <- c("LSOA11","supergroup_class","group_code","group_class")
classf$group_class <- paste0(classf$group_code," ", classf$group_class)
classf <- classf[,c("LSOA11","supergroup_class","group_class")]
all <- left_join(all, classf, by = "LSOA11")
rm(classf)

bounds <- st_read("data-prepared/LSOA_generalised.gpkg")

# Functions

rep_sats <- function(i, dat, Y){
  indexes <- sample(1:nrow(dat), size = nrow(dat) / 2)
  
  model_tree = tree::tree(as.formula(paste0(Y," ~ .")), 
                          data = dat[indexes,], 
                          na.action = na.exclude)
  
  signif_variables <- as.character(summary(model_tree)$used)
  signif_variables <- signif_variables[signif_variables != "(Intercept)"]
  
  estimate_coefs_formula <- paste0(Y," ~ ", paste(signif_variables, collapse = " + "))
  estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = dat, subset = -indexes)
  
  
  sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
  colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
  return(sample_split_coefs)
}

rotate_x <- function(data, labels_vec, rot_angle) {
  plt <- barplot(data, col='steelblue', xaxt="n")
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.6) 
}

produce_hist <- function(x){
  x <- x[!is.na(x)]
  med <- median(x, na.rm = TRUE)
  if(med > 0){
    summ <- sum(x>0)
  } else {
    summ <- sum(x<0)
  }
  return(summ)
}

run_anal <- function(ncores = 4, Y = "MeanDomGas_11_kWh", dat, times = 1000){
  cl <- parallel::makeCluster(ncores)
  res <- pbapply::pblapply(1:times, rep_sats, dat = dat, Y = Y , cl = cl)
  parallel::stopCluster(cl)
  rm(cl)
  
  res_summary <- res %>%
    dplyr::bind_rows() %>%
    summarize_all(produce_hist)
  res_summary <- t(res_summary)
  rotate_x(res_summary[,1], rownames(res_summary), 45)
  return(res_summary)
}

# Calcualte Gas and Electricity per capita

all$gas_kwh_percap <- all$TotDomGas_11_kWh / all$pop2011
all$elec_kwh_percap <- all$TotDomElec_11_kWh / all$pop2011
summary(all$gas_kwh_percap)
summary(all$elec_kwh_percap)

saveRDS(all, paste0(secure_path,"/github-secure-data/lsoa_all2.Rds"))

# Estimate Non Gas Heating
heating <- readRDS("data-prepared/central_heating.Rds") # Not full UK
heating$non_gas <- rowSums(heating[,c("Oil","Solid fuel","Other","Two or more")])
heating <- heating[,c("LSOA11","All","non_gas")]
names(heating) <- c("LSOA11","total_dwellings","non_gas_dwellings")
all <- left_join(all, heating)

median_gas <- median(all$MeanDomGas_11_kWh, na.rm = TRUE)

all$nongas_total <- all$non_gas_dwellings * median_gas
all$nongas_kwh_percap <- all$nongas_total / all$pop2011

# Get Region Bounds
la <- readRDS("data-prepared/la_bounds.Rds")


# Plot General Distribution of ENergy Use
bounds <- left_join(bounds, all[,c("LSOA11","gas_kwh_percap","driving_kwh_percap","elec_kwh_percap","nongas_kwh_percap")])
bounds$country <- substr(bounds$LSOA11,1,1)
bounds <- bounds[bounds$country != "S",]

#Plot 4 types of energy use
tmap_mode("plot")
map1 <- tm_shape(bounds) +
  tm_fill(c("driving_kwh_percap","elec_kwh_percap","gas_kwh_percap","nongas_kwh_percap"),
          palette = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027"),
          breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,35000),
          title = c("Driving","Electricity","Mains Gas","Non-Gas Heating")) +
  tm_shape(la) +
  tm_borders("grey")

tmap_save(map1,"plots/energy_use_map_facets5.png")


# Variables to exclude
exclude_all <- c("LSOA11","DomMet_17","MeanDomElec_17_kWh","TotDomElec_17_kWh","GasMet_11",
                 "TotDomGas_11_kWh","cars_total","cars_miles",
                 "TotDomElec_11_kWh","GasMet_11","MeanDomElec_17_kWh","RUC11",
                 "dense_2017","pop2016","petrol_litres","diesel_litres",
                 "petrol_kwh","diesel_kwh","driving_kwh","petrol_co2",
                 "diesel_co2","petrol_n","diesel_n",
                 "pu5k","p5_12k","po12k",
                 "miles_av_u3","miles_av_o13",
                 "electric diesel_n","hybrid_electric_n","other_n","petrol_n","all_cars_n",
                 "electric diesel_co2", "hybrid_electric_co2",
                 "petrol_emissions","diesel_emissions","petrol_litres","diesel_litres",
                 "mean_bedrooms","Ptn_EE","MeanDomGas_11_kWh","MeanDomElec_11_kWh","nongas_kwh_percap")

exclude_gas <- c("MeanDomElec_11_kWh","dense_2017","pop2016",             
                 "age_av","miles_av_u3","miles_av_o13","pcars_diesel","pmiles_diesel","vans_total",
                 "vans_miles","pmiles_car","pmiles_vans","cars_percap","miles_percap","diesel_n",
                 "petrol_kwh","diesel_kwh","driving_kwh",
                 "driving_kwh_percap")


exclude_elec <- c("MeanDomGas_11_kWh","age_av","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                  "pmiles_car","pmiles_vans","cars_percap","miles_percap",
                  "driving_kwh_percap","X4plusCarHH","X3CarHH","X1CarHH",
                  "X2CarHH","NoCarsHH","gas_kwh_percap")


exclude_drive <- c("MeanDomGas_11_kWh","MeanDomElec_11_kWh","dense_2017","pop2016",
                   "petrol_litres","diesel_litres",
                   "petrol_kwh","diesel_kwh","driving_kwh",
                   "petrol_n","diesel_n","gas_kwh_percap","elec_kwh_percap","nongas_kwh_percap")

# Gas

all_gas <- all[,!names(all) %in% c(exclude_all, exclude_gas)]
all_gas <- all_gas[!is.na(all_gas$gas_kwh_percap),]
all_gas <- all_gas[,!sapply(all_gas, anyNA)]

res_gas <- run_anal(Y = "gas_kwh_percap", dat = all_gas,times = 2000, ncores = 6)
saveRDS(res_gas, "data/importance_gas_tree_final.Rds")

top_gas <- rownames(res_gas)[res_gas[,1] > (max(res_gas[,1]) / 4)]
top_gas <- top_gas[top_gas != "(Intercept)"]

lm_gas <- lm(as.formula(paste0("gas_kwh_percap"," ~ ", 
                               paste(top_gas, collapse = " + "))),
             data = all_gas)
summary(lm_gas)
plot(all_gas$gas_kwh_percap, predict(lm_gas),
     xlim = c(0,18000),
     ylim = c(0,18000),
     xlab = c("Gas consumption kWh per capita"),
     ylab = c("Predicted gas consumption"))
abline(a = 0 , b = 1, col = "red")

## Electric

all_elec <- all[,!names(all) %in% c(exclude_all, exclude_elec)]
all_elec <- all_elec[!is.na(all_elec$elec_kwh_percap),]
all_elec <- all_elec[,!sapply(all_elec, anyNA)]

res_elec <- run_anal(Y = "elec_kwh_percap", dat = all_elec,times = 2000, ncores = 5)
saveRDS(res_elec, "data/importance_elec_tree_final.Rds")

top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

lm_elec <- lm(as.formula(paste0("elec_kwh_percap"," ~ ", 
                                paste(top_elec, collapse = " + "))),
              data = all_elec)
summary(lm_elec)
plot(all_elec$elec_kwh_percap, predict(lm_elec),
     xlim = c(0,7000),
     ylim = c(0,7000),
     xlab = c("Electricity consumption kWh per capita"),
     ylab = c("Predicted electricity consumption"))
abline(a = 0 , b = 1, col = "red")
