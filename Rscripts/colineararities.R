library(ggplot2)
library(GGally)
library(dplyr)
# heatline results
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else if(dir.exists("E:/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "D:/OneDrive - University of Leeds/Data/CREDS Data"
}

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all2.Rds"))
all <- all[substr(all$LSOA11,1,1) == "E",]

emissions <- readRDS("../CarbonCalculator/data-prepared/car_historical_emissions.Rds")
emissions <- emissions[,c("LSOA","AvgCO2_cars_2011")]
all <- left_join(all, emissions, by = c("LSOA11" = "LSOA"))

all$car_km_11 = all$cars_miles * 1.60934
all$car_km_11_percap = all$car_km_11  / all$pop2011
all$car_kgco2_per_cap = all$car_km_11 * (all$AvgCO2_cars_2011 / 1000) / all$pop2011

# Filter out the few LSOAs with loads of cars
all <- all[all$cars_percap < 2,] # more than 99% of LSOA included


plot1 <- ggpairs(data = all[,c("car_kgco2_per_cap","median_household_income","cars_percap","car_km_11_percap","AvgCO2_cars_2011","supergroup_class")],
        mapping = aes(color = supergroup_class),
        columns = c("car_kgco2_per_cap","median_household_income","cars_percap","car_km_11_percap","AvgCO2_cars_2011"),
        columnLabels = c("CO2 per cap","Income","cars per cap","car km per cap","CO2 per km"),
        upper = list(continuous = wrap("cor", size = 2.5))
)
ggsave(plot = plot1, filename = "plots/diagnostic_plot_4.png", width = 12, height = 9)

plot2 <- ggpairs(data = all[,c("car_kgco2_per_cap","median_household_income","acc_town_PTfrequency", "acc_town_cartime","acc_town_p15min_cycle","cars_percap","supergroup_class")],
                 mapping = aes(color = supergroup_class),
                 columns = c("car_kgco2_per_cap","median_household_income","acc_town_PTfrequency", "acc_town_cartime","acc_town_p15min_cycle","cars_percap"),
                 columnLabels = c("CO2 per cap","Income","Town PT frequency", "Town car time","Town % 15 min cycle","cars per cap"),
                 upper = list(continuous = wrap("cor", size = 2.5))
)
ggsave(plot = plot2, filename = "plots/diagnostic_plot_5.png", width = 12, height = 9)


# Old
stop()

ggpairs(data = all[,c("elec_kwh_percap","gas_kwh_percap","driving_kwh_percap","median_household_income","supergroup_class")],
        mapping = aes(color = supergroup_class),
        columns = c("elec_kwh_percap", "gas_kwh_percap", "driving_kwh_percap", "median_household_income"),
        upper = list(continuous = wrap("cor", size = 2.5))
) +
  ggsave("plots/diagnostic_plot.png", width = 12, height = 9)




p2 <- ggpairs(data = all[,c("mean_rooms", "Age_Mean", "cars_percap","supergroup_class")],
        mapping = aes(color = supergroup_class),
        columns = c("mean_rooms", "Age_Mean", "cars_percap", "median_household_income"),
        upper = list(continuous = wrap("cor", size = 2.5))
)

ggsave("plots/diagnostic_plot2.png", plot = p2, width = 12, height = 9)


p3 <- ggpairs(data = all[,c("acc_town_PTfrequency", "acc_town_cartime","acc_town_p15min_cycle","cars_percap","supergroup_class")],
              mapping = aes(color = supergroup_class),
              columns = c("acc_town_PTfrequency", "acc_town_cartime","acc_town_p15min_cycle", "cars_percap"),
              upper = list(continuous = wrap("cor", size = 2.5))
)

ggsave("plots/diagnostic_plot3.png", plot = p3, width = 12, height = 9)


ggpairs(data = all[,c("elec_kwh_percap","gas_kwh_percap","driving_kwh_percap","median_household_income","supergroup_class")],
        mapping = aes(color = supergroup_class),
        columns = c("elec_kwh_percap", "gas_kwh_percap", "driving_kwh_percap", "median_household_income"),
        upper = list(continuous = wrap("cor", size = 2.5))
) +
  ggsave("plots/diagnostic_plot.png", width = 12, height = 9)







