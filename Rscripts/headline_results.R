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
all$supergroup_class <- factor(all$supergroup_class, levels = c("Cosmopolitan student neighbourhoods",
                                                                "Inner city cosmopolitan",
                                                                "Multicultural living",
                                                                "Hard-pressed communities",
                                                                "Industrious communities",
                                                                "Ethnically diverse professionals",
                                                                "Suburban living",
                                                                "Countryside living"))

res_gas <- readRDS("data/importance_gas_tree_final.Rds")
top_gas <- rownames(res_gas)[res_gas[,1] > (max(res_gas[,1]) / 4)]
top_gas <- top_gas[top_gas != "(Intercept)"]


all_gas <- all[!is.na(all$gas_kwh_percap),]
model_gas <- lm(as.formula(paste0("gas_kwh_percap ~ ",paste(top_gas, collapse = " + "))),
                data = all_gas)
summ <- summary(model_gas)
summ
all_gas$gas_predicted <- predict(model_gas)

# png(file = "plots/gas_model.png", width = 1024, height = 768, pointsize = 24)
# plot(all$gas_kwh_percap[!is.na(all$gas_kwh_percap)], predict(model_gas),
#      xlab = "Actual mean gas consumption (kWh)",
#      ylab = "Predicted consumption",
#      xlim = c(0, 40000),
#      ylim = c(0, 40000),
#      pch = 20)
# abline(0,1, col = "red")
# dev.off()

# coff <- summ$coefficients


# lm <- lm(all_gas$gas_kwh_percap ~ all_gas$mean_rooms)
# all_gas$gas_resid <- lm$residuals
# 
# plot(all_gas$Outright, all_gas$gas_resid)

ggplot(all_gas, aes(gas_kwh_percap, gas_predicted, group = supergroup_class)) +
  geom_hex(alpha = 0.9) +
  scale_fill_gradientn(colours = c("lightblue","yellow","red")) +
  facet_wrap(~supergroup_class, ncol = 2) + 
  coord_fixed() +
  theme(legend.position="top") +
  xlab("Actual mean gas consumption (kWh)") +
  ylab("Predicted consumption (kWh)") +
  scale_x_continuous(limits=c(0, 18000), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 10000), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/gas_model3.png", width = 4, height = 6, 
         dpi = 600, scale = 1.8)
  

## Electricity #####

res_elec <- readRDS("data/importance_elec_tree_final.Rds")
top_elec <- rownames(res_elec)[res_elec[,1] > (max(res_elec[,1]) / 4)]
top_elec <- top_elec[top_elec != "(Intercept)"]

model_elec <- lm(as.formula(paste0("elec_kwh_percap ~ ",paste(top_elec, collapse = " + "))),
                data = all)
summ <- summary(model_elec)
summ
all_elec <- all[!is.na(all$elec_kwh_percap),]
all_elec$elec_predicted <- predict(model_elec)

ggplot(all_elec, aes(elec_kwh_percap, elec_predicted, group = supergroup_class)) +
  geom_hex(alpha = 0.9) +
  scale_fill_gradientn(colours = c("lightblue","yellow","red")) +
  facet_wrap(~supergroup_class, ncol = 2) + 
  coord_fixed() +
  theme(legend.position="top") +
  xlab("Actual mean electricity consumption (kWh)") +
  ylab("Predicted consumption (kWh)") +
  scale_x_continuous(limits=c(0, 7999), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 4500), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/electricity_model3.png", width = 4, height = 6, 
         dpi = 600, scale = 1.8)



# png(file = "plots/electricity_model.png", width = 1024, height = 768, pointsize = 24)
# plot(all$elec_kwh_percap[!is.na(all$elec_kwh_percap)], predict(model_elec),
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

all_miles$km_percap <- all_miles$miles_percap * 1.60934
all_miles$km_predicted <- all_miles$miles_predicted * 1.60934

ggplot(all_miles, aes(km_percap, km_predicted, group = supergroup_class)) +
  geom_hex(alpha = 0.9) +
  scale_fill_gradientn(colours = c("lightblue","yellow","red")) +
  facet_wrap(~supergroup_class, ncol = 2) + 
  #coord_fixed() +
  theme(legend.position="top") +
  xlab("Actual mean km per capita") +
  ylab("Predicted km per capita") +
  scale_x_continuous(limits=c(0, 45000), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 34000), expand = c(0, 0)) +
  geom_abline(intercept = 0, colour = "red") +
  theme(legend.background = element_rect(fill=alpha('white', 0.4))) +
  guides(colour = guide_legend(override.aes = list(size=2, shape = 19))) +
  labs(colour = "") +
  ggsave("plots/miles_model2.png", width = 4, height = 6, 
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
all$total <- rowSums(all[,c("elec_kwh_percap", "gas_kwh_percap", "driving_kwh_percap","nongas_kwh_percap")], na.rm=TRUE) 
order_sum <- all %>%
  group_by(group_class) %>%
  summarise(all = median(total, na.rm = TRUE),
            gas = median(gas_kwh_percap, na.rm = TRUE),
            elec = median(elec_kwh_percap, na.rm = TRUE),
            drive = median(driving_kwh_percap, na.rm = TRUE),
            total_q1 = quantile(total, probs = 0.25, na.rm = TRUE),
            total_q3 = quantile(total, probs = 0.75, na.rm = TRUE),
            numb_gas_na = sum(is.na(gas_kwh_percap)),
            iqr = IQR(total, na.rm = TRUE),
            outlier_over = sum(total > 1.5 * iqr + total_q3),
            outlier_under = sum(total < total_q1 - 1.5 * iqr),
            income = median(median_household_income, na.rm = TRUE)
            )

library(tidyr)
all_box <- all[,c("elec_kwh_percap", "gas_kwh_percap", "driving_kwh_percap","nongas_kwh_percap","group_class")]
names(all_box) = c("Electricity","Gas","Driving","Non-Gas","Class")
all_box <- pivot_longer(all_box, cols = c("Electricity","Gas","Driving","Non-Gas"))
# lvls <- c("1a Cosmopolitan student neighbourhoods",
#           "6a Inner city cosmopolitan",
#           "4d Hard-pressed flat dwellers",
#           "4a Challenged white communities",
#           "4c Hampered neighbourhoods",
#           "4b Constrained renters",
#           "7a Urban cultural mix",
#           "7b Young ethnic communities",
#           "5e Primary sector workers",
#           "5d Endeavouring social renters",
#           "5b Aspiring urban households",
#           "5a Ageing urban communities",
#           "5c Comfortable neighbourhoods",
#           "3d Households in terraces and flats",
#           "3a Achieving neighbourhoods",
#           "3b Asian traits",
#           "3c Highly qualified professionals",
#           "8b Ageing suburbanites",
#           "8c Comfortable suburbia",
#           "8a Affluent communities",
#           "2c Remoter communities",
#           "2d Rural traits",
#           "2a Ageing rural neighbourhoods",  
#           "2b Prospering countryside life")

lvls <- c("1a Cosmopolitan student neighbourhoods", "6a Inner city cosmopolitan",
          "4d Hard-pressed flat dwellers","7b Young ethnic communities",
          "7a Urban cultural mix","4a Challenged white communities",
          "4c Hampered neighbourhoods","4b Constrained renters",
          "3d Households in terraces and flats","5e Primary sector workers",
          "5d Endeavouring social renters","5b Aspiring urban households",
          "5a Ageing urban communities","5c Comfortable neighbourhoods",
          "3a Achieving neighbourhoods","3c Highly qualified professionals",
          "3b Asian traits","8c Comfortable suburbia",
          "8b Ageing suburbanites","2d Rural traits",
          "2c Remoter communities","2a Ageing rural neighbourhoods",
          "8a Affluent communities","2b Prospering countryside life")


all_box$labs <- factor(all_box$Class,
                       levels = lvls ,ordered = TRUE)


ggplot(all_box, aes(labs, value, fill = name )) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 11000), expand = c(0, 0)) +
  coord_flip() +
  ylab("Energy use per capita (kWh)") +
  xlab("Area Classification") +
  theme(legend.position = c(0.9, 0.15), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  labs(fill = "Energy Type") +
  ggsave("plots/box_energy.png", width = 6, height = 4, 
         dpi = 600, scale = 1.8)


all_box2 <- all[,c("elec_kwh_percap", "gas_kwh_percap", "driving_kwh_percap","nongas_kwh_percap","group_class", "supergroup_class")]
names(all_box2) = c("Electricity","Gas","Driving","Non-Gas","Class","supergroup_class")
all_box2$Electricity <- all_box2$Electricity * 0.2556
all_box2$Gas <- all_box2$Gas * 0.20428
all_box2$Driving <- all_box2$Driving * 0.24603
all_box2$`Non-Gas` <- all_box2$`Non-Gas` * 0.28492
all_box2$total_emissions_per_cap = rowSums(all_box2[,c("Electricity","Gas","Driving","Non-Gas")], na.rm=TRUE)
#all_box2 <- pivot_longer(all_box2, cols = "total_emissions_per_cap")
all_box2$labs <- factor(all_box2$Class,
                       levels = lvls ,ordered = TRUE)

ggplot(all_box2, aes(labs, total_emissions_per_cap, fill = supergroup_class)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 7000), expand = c(0, 0)) +
  coord_flip() +
  ylab("GHG Emissions per capita (kg CO2e)") +
  xlab("Area Classification") +
  theme(legend.position = "none") +
  ggsave("plots/box_carbon.png", width = 6, height = 4, 
         dpi = 600, scale = 1.8)

# foo <- all_box2 %>%
#   group_by(Class) %>%
#   summarise(emissions = median(total_emissions_per_cap))
foo <- foo[order(foo$emissions), ]

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
