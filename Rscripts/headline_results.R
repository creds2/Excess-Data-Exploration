# heatline results
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}

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
summ <- summary(model_gas)
summ

png(file = "plots/gas_model.png", width = 1024, height = 768, pointsize = 24)
plot(all$MeanDomGas_11_kWh[!is.na(all$MeanDomGas_11_kWh)], predict(model_gas),
     xlab = "Actual mean gas consumption (kWh)",
     ylab = "Predicted consumption",
     xlim = c(0, 40000),
     ylim = c(0, 40000),
     pch = 20)
abline(0,1, col = "red")
dev.off()

coff <- summ$coefficients

all_gas <- all[!is.na(all$MeanDomGas_11_kWh),]
lm <- lm(all_gas$MeanDomGas_11_kWh ~ all_gas$mean_rooms)
all_gas$gas_resid <- lm$residuals

plot(all_gas$Outright, all_gas$gas_resid)



res_elec <- readRDS("data/importance_elec_tree.Rds")
top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

model_elec <- lm(as.formula(paste0("MeanDomElec_11_kWh ~ ",paste(top_elec, collapse = " + "))),
                data = all)
summ <- summary(model_elec)
summ

png(file = "plots/electricity_model.png", width = 1024, height = 768, pointsize = 24)
plot(all$MeanDomElec_11_kWh[!is.na(all$MeanDomElec_11_kWh)], predict(model_elec),
     xlab = "Actual mean electricity consumption (kWh)",
     ylab = "Predicted consumption",
     xlim = c(0, 17000),
     ylim = c(0, 17000),
     pch = 20)
abline(0,1, col = "red")
dev.off()
coff <- summ$coefficients

res_miles <- readRDS("data/importance_miles_per_cap_tree.Rds")
top_miles <- rownames(res_miles)[res_miles[,1] > (max(res_miles[,1]) / 4)]
top_miles <- top_miles[top_miles != "(Intercept)"]

all_miles <- all[!is.na(all$miles_percap),]
all_miles <- all_miles[!is.na(all$acc_town_PTfrequency),]
model_miles <- lm(as.formula(paste0("miles_percap ~ ",paste(top_miles, collapse = " + "))),
                 data = all_miles)
summ <- summary(model_miles)
summ

png(file = "plots/miles_model.png", width = 1024, height = 768, pointsize = 24)
plot(all_miles$miles_percap, predict(model_miles),
     xlab = "Actual mean miles per capita",
     ylab = "Predicted miles per capita",
     xlim = c(0, 30000),
     ylim = c(0, 30000),
     pch = 20)
abline(0,1, col = "red")
dev.off()


coff <- summ$coefficients


res_cars <- readRDS("data/importance_cars_per_cap_tree.Rds")
res_cars2 <- readRDS("data/importance_cars_per_cap_tree2.Rds")
top_cars <- rownames(res_cars)[res_cars[,1] > (max(res_cars[,1]) / 4)]
top_cars <- top_cars[top_cars != "(Intercept)"]

all_cars <- all[!is.na(all$cars_percap),]
all_cars <- all_cars[!is.na(all$acc_town_PTfrequency),]
model_cars <- lm(as.formula(paste0("cars_percap ~ ",paste(top_cars, collapse = " + "))),
                  data = all_cars)
summ <- summary(model_cars)
summ

png(file = "plots/cars_model.png", width = 1024, height = 768, pointsize = 24)
plot(all_cars$cars_percap, predict(model_cars),
     xlab = "Actual mean cars per capita",
     ylab = "Predicted cars per capita",
     xlim = c(0, 3.6),
     ylim = c(0, 3.6),
     pch = 20)
abline(0,1, col = "red")
dev.off()


coff <- summ$coefficients


# realthispohip between cars and miles
summary(all$cars_miles / all$cars_total)
