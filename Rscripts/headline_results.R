library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
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

classf <- readRDS("data-prepared/area_classification.Rds")
classf <- classf[,c("SOA Code","Supergroup Name","Group Code","Group Name")]
names(classf) <- c("LSOA11","supergroup_class","group_code","group_class")
classf$group_class <- paste0(classf$group_code," ", classf$group_class)
classf <- classf[,c("LSOA11","supergroup_class","group_class")]
all <- left_join(all, classf, by = "LSOA11")


res_gas <- readRDS("data/importance_gas_tree.Rds")
top_gas <- rownames(res_gas)[res_gas[,1] > (max(res_gas[,1]) / 4)]
top_gas <- top_gas[top_gas != "(Intercept)"]


all_gas <- all[!is.na(all$MeanDomGas_11_kWh),]
model_gas <- lm(as.formula(paste0("MeanDomGas_11_kWh ~ ",paste(top_gas, collapse = " + "))),
                data = all_gas)
summ <- summary(model_gas)
summ
all_gas$gas_predicted <- predict(model_gas)

# png(file = "plots/gas_model.png", width = 1024, height = 768, pointsize = 24)
# plot(all$MeanDomGas_11_kWh[!is.na(all$MeanDomGas_11_kWh)], predict(model_gas),
#      xlab = "Actual mean gas consumption (kWh)",
#      ylab = "Predicted consumption",
#      xlim = c(0, 40000),
#      ylim = c(0, 40000),
#      pch = 20)
# abline(0,1, col = "red")
# dev.off()

# coff <- summ$coefficients


# lm <- lm(all_gas$MeanDomGas_11_kWh ~ all_gas$mean_rooms)
# all_gas$gas_resid <- lm$residuals
# 
# plot(all_gas$Outright, all_gas$gas_resid)

ggplot(all_gas, aes(MeanDomGas_11_kWh, gas_predicted, colour = supergroup_class)) +
  geom_point(size = 0.1, shape = 15) +
  xlab("Actual mean gas consumption (kWh)") +
  ylab("Predicted consumption (kWh)") +
  scale_x_continuous(limits=c(0, 41100), expand = c(0, 0)) +
  scale_y_continuous(limits=c(5000, 31000), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.position = c(0.12, 0.8), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/gas_model.png", width = 6, height = 3, 
         dpi = 600, scale = 1.8)
  

## Electricity #####

res_elec <- readRDS("data/importance_elec_tree.Rds")
top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

model_elec <- lm(as.formula(paste0("MeanDomElec_11_kWh ~ ",paste(top_elec, collapse = " + "))),
                data = all)
summ <- summary(model_elec)
summ
all_elec <- all[!is.na(all$MeanDomElec_11_kWh),]
all_elec$elec_predicted <- predict(model_elec)

ggplot(all_elec, aes(MeanDomElec_11_kWh, elec_predicted, colour = supergroup_class)) +
  geom_point(size = 0.1, shape = 15) +
  xlab("Actual mean electricity consumption (kWh)") +
  ylab("Predicted consumption (kWh)") +
  scale_x_continuous(limits=c(0, 17000), expand = c(0, 0)) +
  scale_y_continuous(limits=c(2000, 8000), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.position = c(0.12, 0.8), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/electricity_model.png", width = 6, height = 3, 
         dpi = 600, scale = 1.8)



# png(file = "plots/electricity_model.png", width = 1024, height = 768, pointsize = 24)
# plot(all$MeanDomElec_11_kWh[!is.na(all$MeanDomElec_11_kWh)], predict(model_elec),
#      xlab = "Actual mean electricity consumption (kWh)",
#      ylab = "Predicted consumption",
#      xlim = c(0, 17000),
#      ylim = c(0, 17000),
#      pch = 20)
# abline(0,1, col = "red")
# dev.off()
# coff <- summ$coefficients

# Miles per capita ########################################## 

res_miles <- readRDS("data/importance_miles_per_cap_tree.Rds")
top_miles <- rownames(res_miles)[res_miles[,1] > (max(res_miles[,1]) / 4)]
top_miles <- top_miles[top_miles != "(Intercept)"]

all_miles <- all[!is.na(all$miles_percap),]
all_miles <- all_miles[!is.na(all$acc_town_PTfrequency),]
model_miles <- lm(as.formula(paste0("miles_percap ~ ",paste(top_miles, collapse = " + "))),
                 data = all_miles)
summ <- summary(model_miles)
summ
all_miles$miles_predicted <- predict(model_miles)

ggplot(all_miles, aes(miles_percap, miles_predicted, colour = supergroup_class)) +
  geom_point(size = 0.1, shape = 15) +
  xlab("Actual mean miles per capita") +
  ylab("Predicted miles per capita") +
  scale_x_continuous(limits=c(0, 28000), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 22000), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.position = c(0.12, 0.8), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/miles_model.png", width = 6, height = 3, 
         dpi = 600, scale = 1.8)

# png(file = "plots/miles_model.png", width = 1024, height = 768, pointsize = 24)
# plot(all_miles$miles_percap, predict(model_miles),
#      xlab = "Actual mean miles per capita",
#      ylab = "Predicted miles per capita",
#      xlim = c(0, 30000),
#      ylim = c(0, 30000),
#      pch = 20)
# abline(0,1, col = "red")
# dev.off()
# 
# 
# coff <- summ$coefficients


res_cars <- readRDS("data/importance_cars_per_cap_tree.Rds")
#res_cars <- readRDS("data/importance_cars_per_cap_tree2.Rds")
top_cars <- rownames(res_cars)[res_cars[,1] > (max(res_cars[,1]) / 4)]
top_cars <- top_cars[top_cars != "(Intercept)"]

all_cars <- all[!is.na(all$cars_percap),]
all_cars <- all_cars[!is.na(all$acc_town_PTfrequency),]
model_cars <- lm(as.formula(paste0("cars_percap ~ ",paste(top_cars, collapse = " + "))),
                  data = all_cars)
summ <- summary(model_cars)
summ

all_cars$cars_predicted <- predict(model_cars)

#n.b exclude 2 LSOA
ggplot(all_cars, aes(cars_percap, cars_predicted, colour = supergroup_class)) +
  geom_point(size = 0.1, shape = 15) +
  xlab("Actual mean cars per capita") +
  ylab("Predicted cars per capita") +
  scale_x_continuous(limits=c(0, 2.01), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 2.01), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.position = c(0.12, 0.8), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/cars_model.png", width = 6, height = 3, 
         dpi = 600, scale = 1.8)


# png(file = "plots/cars_model.png", width = 1024, height = 768, pointsize = 24)
# plot(all_cars$cars_percap, predict(model_cars),
#      xlab = "Actual mean cars per capita",
#      ylab = "Predicted cars per capita",
#      xlim = c(0, 3.6),
#      ylim = c(0, 3.6),
#      pch = 20)
# abline(0,1, col = "red")
# dev.off()
# 
# 
# coff <- summ$coefficients


# realthispohip between cars and miles
summary(all$cars_miles / all$cars_total)



######## Box and wisker for open geodeomgraphci glasses
all$total <- rowSums(all[,c("MeanDomElec_11_kWh", "MeanDomGas_11_kWh", "driving_kwh_percap")], na.rm=TRUE) 
order_sum <- all %>%
  group_by(group_class) %>%
  summarise(all = median(total, na.rm = TRUE),
            gas = median(MeanDomGas_11_kWh, na.rm = TRUE),
            elec = median(MeanDomElec_11_kWh, na.rm = TRUE),
            drive = median(driving_kwh_percap, na.rm = TRUE),
            total_q1 = quantile(total, probs = 0.25, na.rm = TRUE),
            total_q3 = quantile(total, probs = 0.75, na.rm = TRUE),
            numb_gas_na = sum(is.na(MeanDomGas_11_kWh)),
            iqr = IQR(total, na.rm = TRUE),
            outlier_over = sum(total > 1.5 * iqr + total_q3),
            outlier_under = sum(total < total_q1 - 1.5 * iqr),
            income = median(median_household_income, na.rm = TRUE)
            )

library(tidyr)
all_box <- all[,c("MeanDomElec_11_kWh", "MeanDomGas_11_kWh", "driving_kwh_percap","group_class")]
names(all_box) = c("Electricity","Gas","Driving","Class")
all_box <- pivot_longer(all_box, cols = c("Electricity","Gas","Driving"))
lvls <- c("4d Hard-pressed flat dwellers",
          "4a Challenged white communities",
          "4b Constrained renters",
          "4c Hampered neighbourhoods",
          "6a Inner city cosmopolitan",
          "1a Cosmopolitan student neighbourhoods",
          "5e Primary sector workers",
          "5d Endeavouring social renters",
          "5b Aspiring urban households",
          "5a Ageing urban communities",
          "5c Comfortable neighbourhoods",
          "7a Urban cultural mix",
          "7b Young ethnic communities",
          "3d Households in terraces and flats",
          "3a Achieving neighbourhoods",
          "3b Asian traits",
          "3c Highly qualified professionals",
          "2c Remoter communities",
          "2d Rural traits",
          "2a Ageing rural neighbourhoods",  
          "2b Prospering countryside life",
          "8b Ageing suburbanites",
          "8c Comfortable suburbia",
          "8a Affluent communities")

all_box$labs <- factor(all_box$Class,
                       levels = lvls ,ordered = TRUE)


ggplot(all_box, aes(labs, value, fill = name )) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 30000), expand = c(0, 0)) +
  coord_flip() +
  ylab("Energy use per capita (kWh)") +
  xlab("Area Classification") +
  theme(legend.position = c(0.8, 0.1), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  labs(fill = "Energy Type") +
  ggsave("plots/box_energy.png", width = 6, height = 4, 
         dpi = 600, scale = 1.8)


all_box2 <- all[,c("MeanDomElec_11_kWh", "MeanDomGas_11_kWh", "driving_kwh_percap","group_class", "supergroup_class")]
names(all_box2) = c("Electricity","Gas","Driving","Class","supergroup_class")
all_box2$Electricity <- all_box2$Electricity * 0.2556
all_box2$Gas <- all_box2$Gas * 0.20428
all_box2$Driving <- all_box2$Driving * 0.25
all_box2$total_emissions_per_cap = rowSums(all_box2[,c("Electricity","Gas","Driving")], na.rm=TRUE)
#all_box2 <- pivot_longer(all_box2, cols = "total_emissions_per_cap")
all_box2$labs <- factor(all_box2$Class,
                       levels = lvls ,ordered = TRUE)

ggplot(all_box2, aes(labs, total_emissions_per_cap, fill = supergroup_class)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 12500), expand = c(0, 0)) +
  coord_flip() +
  ylab("GHG Emissions per capita (kg CO2e)") +
  xlab("Area Classification") +
  theme(legend.position = "none") +
  ggsave("plots/box_carbon.png", width = 6, height = 4, 
         dpi = 600, scale = 1.8)

#### Map outliers
order_sum$out_over <- order_sum$total_q3 + 1.5 * order_sum$iqr
order_sum_join <- order_sum[,c("group_class","out_over","total_q3")]
all <- left_join(all, order_sum_join, by = c("group_class"))
all_out <- all[all$total > all$out_over,]
all_q4 <- all[all$total > all$total_q3,]

sum(all_q4$total * all_q4$pop2011) / sum(all$total * all$pop2011) * 100
sum(all_q4$pop2011) / sum(all$pop2011) * 100

bounds = read_sf("data-prepared/LSOA_forplots.gpkg")
bounds$country = substr(bounds$LSOA11, 1, 1)
bounds = bounds[bounds$country != "S",]

ru <- readRDS("data-prepared/ruralurban.Rds")
ru <- left_join(bounds, ru, by = "LSOA11")
urban <- ru[ru$RUC11 %in% c("Urban major conurbation","Urban city and town","Urban minor conurbation"),]
urban <- group_by(urban, RUC11) %>%
  summarise()

all_out = left_join(all_out, bounds, by = c("LSOA11"))
all_out = st_as_sf(all_out)
map <- tm_shape(all_out) +
  tm_fill(col = "supergroup_class",
          title = "LSOA Supergroup") +
  tm_shape(urban) +
  tm_borders(col = "black")
tmap_save(map, "plots/energy_outliers.png",
          dpi = 600)
