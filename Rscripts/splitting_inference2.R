# Librarys

library(tree)
library(dplyr)
library(ggplot2)

# Import Data  -----------------------------------------------
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))

names(all) <- gsub(".","_", names(all), fixed = TRUE)
names(all) <- gsub(" ","_", names(all), fixed = TRUE)


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
                 "mean_bedrooms","Ptn_EE")

exclude_gas <- c("MeanDomElec_11_kWh","dense_2017","pop2016",             
                 "age_av","miles_av_u3","miles_av_o13","pcars_diesel","pmiles_diesel","vans_total",
                 "vans_miles","pmiles_car","pmiles_vans","cars_percap","miles_percap","diesel_n",
                 "petrol_kwh","diesel_kwh","driving_kwh",
                 "driving_kwh_percap")


exclude_elec <- c("MeanDomGas_11_kWh","age_av","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                  "pmiles_car","pmiles_vans","cars_percap","miles_percap",
                  "driving_kwh_percap","X4plusCarHH","X3CarHH","X1CarHH",
                  "X2CarHH","NoCarsHH")


exclude_drive <- c("MeanDomGas_11_kWh","MeanDomElec_11_kWh","dense_2017","pop2016",
                  "petrol_litres","diesel_litres",
                  "petrol_kwh","diesel_kwh","driving_kwh",
                  "petrol_n","diesel_n")


# Run Analysis

## Gas

all_gas <- all[,!names(all) %in% c(exclude_all, exclude_gas)]
all_gas <- all_gas[!is.na(all_gas$MeanDomGas_11_kWh),]
all_gas <- all_gas[,!sapply(all_gas, anyNA)]

res_gas <- run_anal(Y = "MeanDomGas_11_kWh", dat = all_gas,times = 2000, ncores = 5)
saveRDS(res_gas, "data/importance_gas_tree.Rds")

top_gas <- rownames(res_gas)[res_gas[,1] > (max(res_gas[,1]) / 4)]
top_gas <- top_gas[top_gas != "(Intercept)"]

lm_gas <- lm(as.formula(paste0("MeanDomGas_11_kWh"," ~ ", 
                               paste(top_gas, collapse = " + "))),
             data = all_gas)
summary(lm_gas)
plot(all_gas$MeanDomGas_11_kWh, predict(lm_gas),
     xlim = c(0,40000),
     ylim = c(0,40000),
     xlab = c("Gas consumption kWh per capita"),
     ylab = c("Predicted gas consumption"))
abline(a = 0 , b = 1, col = "red")

## Electric

all_elec <- all[,!names(all) %in% c(exclude_all, exclude_elec)]
all_elec <- all_elec[!is.na(all_elec$MeanDomElec_11_kWh),]
all_elec <- all_elec[,!sapply(all_elec, anyNA)]

res_elec <- run_anal(Y = "MeanDomElec_11_kWh", dat = all_elec,times = 2000, ncores = 5)
saveRDS(res_elec, "data/importance_elec_tree.Rds")

top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

lm_elec <- lm(as.formula(paste0("MeanDomElec_11_kWh"," ~ ", 
                               paste(top_elec, collapse = " + "))),
             data = all_elec)
summary(lm_elec)
plot(all_elec$MeanDomElec_11_kWh, predict(lm_elec),
     xlim = c(0,20000),
     ylim = c(0,20000),
     xlab = c("Electricity consumption kWh per capita"),
     ylab = c("Predicted electricity consumption"))
abline(a = 0 , b = 1, col = "red")

## Driving

all_drive <- all[,!names(all) %in% c(exclude_all, exclude_drive)]
all_drive <- all_drive[!is.na(all_drive$driving_kwh_percap),]
all_drive <- all_drive[,!sapply(all_drive, anyNA)]

res_drive <- run_anal(Y = "driving_kwh_percap", dat = all_drive,times = 2000, ncores = 5)
saveRDS(res_drive, "data/importance_drive_tree.Rds")

top_drive <- rownames(res_drive)[res_drive[,1] > (max(res_drive[,1]) / 4)]
top_drive <- top_drive[top_drive != "(Intercept)"]

lm_drive <- lm(as.formula(paste0("driving_kwh_percap"," ~ ", 
                                paste(top_drive, collapse = " + "))),
              data = all_drive)
summary(lm_drive)
plot(all_drive$driving_kwh_percap, predict(lm_drive),
     xlim = c(0,35000),
     ylim = c(0,35000),
     xlab = c("Driving energy consumption kWh per capita"),
     ylab = c("Predicted driving consumption"))
abline(a = 0 , b = 1, col = "red")

# try again to predict miles per cap

all_drive$driving_kwh_percap <- NULL

res_miles <- run_anal(Y = "miles_percap", dat = all_drive,times = 2000, ncores = 5)
saveRDS(res_miles, "data/importance_miles_per_cap_tree.Rds")

top_miles <- rownames(res_miles)[res_miles[,1] > (max(res_miles[,1]) / 4)]
top_miles <- top_miles[top_miles != "(Intercept)"]

lm_miles <- lm(as.formula(paste0("miles_percap"," ~ ", 
                                 paste(top_miles, collapse = " + "))),
               data = all_drive)
summary(lm_miles)
plot(all_drive$miles_percap, predict(lm_miles),
     xlim = c(0,42000),
     ylim = c(0,42000),
     xlab = c("Miles per capita"),
     ylab = c("Predicted miles per capita"))
abline(a = 0 , b = 1, col = "red")

# try again to predict cars per cap
exclude_cars <- c("miles_percap","NoCarsHH","X1CarHH","X2CarHH","X3CarHH","X4plusCarHH",
                  "mean_rooms","Whole_House_Detached","Mortgage","Outright","Rented_Landlord",
                  "Occupancy_Rooms","Whole_House_Semi","Occupancy_Bedrooms",
                  "NoCH","Flat_PurposeBuilt","All_Grade","pHeating_None",
                  "mean_household_size")


all_drive <- all_drive[!names(all_drive) %in% c(exclude_cars)]




res_cars <- run_anal(Y = "cars_percap", dat = all_drive,times = 2000, ncores = 5)
saveRDS(res_cars, "data/importance_cars_per_cap_tree.Rds")

top_cars <- rownames(res_cars)[res_cars[,1] > (max(res_cars[,1]) / 4)]
top_cars <- top_cars[top_cars != "(Intercept)"]

lm_cars <- lm(as.formula(paste0("cars_percap"," ~ ", 
                                 paste(top_cars, collapse = " + "))),
               data = all_drive)
summary(lm_cars)
plot(all_drive$cars_percap, predict(lm_cars),
     xlim = c(0,2.5),
     ylim = c(0,2.5),
     xlab = c("cars per capita"),
     ylab = c("Predicted cars per capita"))
abline(a = 0 , b = 1, col = "red")

stop()

# all about cars per cap, does that reflect internal variation or not?
# make a plot of cars per capita vs proportion of 3 cars
lm_cpc <- lm(all$miles_percap ~ all$cars_percap)
summary(lm_cpc)

library(gridExtra)
cars <- all[,c("cars_percap","NoCarsHH","X1CarHH","X2CarHH","X3CarHH","X4plusCarHH")]
cars$max <- apply(cars, 1, max)


p0 <- ggplot(cars, aes(x = cars_percap, y = NoCarsHH)) +
  geom_point() +
  geom_smooth() +
  ylab("% No car") +
  xlab("Cars per capita") +
  xlim(0, 2) +
  ylim(0, 80)
  
p1 <- ggplot(cars, aes(x = cars_percap, y = X1CarHH)) +
  geom_point() +
  geom_smooth() +
  ylab("% One car") +
  xlab("Cars per capita") +
  xlim(0, 2) +
  ylim(0, 80)

p2 <- ggplot(cars, aes(x = cars_percap, y = X2CarHH)) +
  geom_point() +
  geom_smooth() +
  ylab("% Two car") +
  xlab("Cars per capita") +
  xlim(0, 2) +
  ylim(0, 80)

p3 <- ggplot(cars, aes(x = cars_percap, y = X3CarHH)) +
  geom_point() +
  geom_smooth() +
  ylab("% Three car") +
  xlab("Cars per capita") +
  xlim(0, 2) +
  ylim(0, 80)

p4 <- ggplot(cars, aes(x = cars_percap, y = X4plusCarHH)) +
  geom_point() +
  geom_smooth() +
  ylab("% 4 plus car") +
  xlab("Cars per capita") +
  xlim(0, 2) +
  ylim(0, 80)

pmax <- ggplot(cars, aes(x = cars_percap, y = max)) +
  geom_point() +
  geom_smooth() +
  ylab("% in the modal group") +
  xlab("Cars per capita") +
  xlim(0, 2) +
  ylim(0, 80)


grid.arrange(p0, p1, p2,p3, p4, pmax, nrow = 3) 
# need to manuall save plot

# try again to predict cars per cap


all_sub <- all_noNA[,!names(all_noNA) %in% c("LSOA11","MeanDomGas_11_kWh","dense_2017","pop2016",
                                             "petrol_litres","diesel_litres",
                                             "petrol_kwh","diesel_kwh","driving_kwh","petrol_co2","diesel_co2",
                                             "petrol_n","diesel_n","driving_kwh_percap",
                                             "miles_av_u3", "miles_av_o13","pu5k","p5_12k","po12k",
                                             "miles_percap", "NoCarsHH","X1CarHH","X2CarHH","X3CarHH","X4plusCarHH",
                                             "all_cars_n")]

names(all_sub) <- gsub(" ","_",names(all_sub))
ncores = 4
cl <- parallel::makeCluster(ncores)
res <- pbapply::pblapply(1:1000, rep_sats, dat = all_sub, Y = "cars_percap", cl = cl)
parallel::stopCluster(cl)
rm(cl)


res_summary <- res %>% 
  dplyr::bind_rows() %>%
  summarize_all(function(x) sum(!is.na(x)))
res_summary <- t(res_summary)
plot(res_summary)

saveRDS(res_summary, "data/importance_cars_per_cap_tree.Rds")

ggplot(all, aes(T2W_Car, cars_percap, color = RUC11)) +
  geom_point() +
  facet_wrap(~ RUC11) +
  ylim(0,2)


# predic mean_rooms

all_sub <- all_noNA[,!names(all_noNA) %in% c("LSOA11","MeanDomGas_11_kWh","dense_2017","pop2016",
                                             "petrol_litres","diesel_litres",
                                             "petrol_kwh","diesel_kwh","driving_kwh","petrol_co2","diesel_co2",
                                             "petrol_n","diesel_n","driving_kwh_percap",
                                             "miles_av_u3", "miles_av_o13","pu5k","p5_12k","po12k",
                                             "miles_percap", "NoCarsHH","X1CarHH","X2CarHH","X3CarHH","X4plusCarHH",
                                             "all_cars_n","cars_percap","mean_bedrooms")]

names(all_sub) <- gsub(" ","_",names(all_sub))
ncores = 4
cl <- parallel::makeCluster(ncores)
res <- pbapply::pblapply(1:1000, rep_sats, dat = all_sub, Y = "mean_rooms", cl = cl)
parallel::stopCluster(cl)
rm(cl)

res_summary <- res %>% 
  dplyr::bind_rows() %>%
  summarize_all(function(x) sum(!is.na(x)))
res_summary <- t(res_summary)
plot(res_summary)

saveRDS(res_summary, "data/mean_rooms.Rds")
