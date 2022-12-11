# Briefing Note Plots

# Librarys

library(tmap)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
tmap_mode("plot")

# Import Data  -----------------------------------------------
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/"
} else {
  secure_path <- "D:/OneDrive - University of Leeds/"
}

la <- readRDS("data-prepared/la_bounds.Rds")

bounds <- st_read("data-prepared/LSOA_generalised.gpkg")
elec <- readxl::read_excel("data-input/Energy Consumption/LSOA_domestic_elec_2010-18.xlsx", sheet = "2018")
names(elec) <- as.character(elec[1,])
elec <- elec[,c(6,8)]
elec <- elec[2:nrow(elec),]
names(elec) <- c("LSOA11","elec_total")


gas <- readxl::read_excel("data-input/Energy Consumption/LSOA_domestic_gas_2010-18.xlsx", sheet = "2018")
names(gas) <- as.character(gas[1,])
gas <- gas[,c(6,8)]
gas <- gas[2:nrow(gas),]
names(gas) <- c("LSOA11","gas_total")

mot <- read.csv(paste0(secure_path,"Data/CREDS Data/Tim Share/From Tim/MOT Data RACv9.3/MOT Data RACv9.3 LSOAoutputs_2011.csv"), stringsAsFactors = FALSE)
names(mot) <- c("LSOA11", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                "pmiles_car","pmiles_vans","cars_percap","miles_percap")

#mot <- mot[,c("LSOA11","miles_percap")]
population <- readRDS("data-prepared/population.Rds")

dir.create("temp")
unzip("../Secure-Data/Excess/Experian.zip", exdir = "temp")
experian <- read.csv("temp/UKDA-5738-csv/csv/2011-experian-data.csv", stringsAsFactors = FALSE)
unlink("temp", recursive = T)
names(experian) <- experian[4,]
experian <- experian[5:nrow(experian),]
experian <- experian[,c("GeographyValue","Median_(H) Household Income Value")]
names(experian) <- c("LSOA11","median_household_income")

emissions <- readRDS(paste0(secure_path,"Data/CREDS Data/github-secure-data/lsoa_emissions.Rds"))
names(emissions)[1] <- "LSOA11"

heating <- readRDS("data-prepared/central_heating.Rds") # Not full UK
heating$non_gas <- rowSums(heating[,c("Oil","Solid fuel","Other","Two or more")])
heating <- heating[,c("LSOA11","All","non_gas")]
names(heating) <- c("LSOA11","total_dwellings","non_gas_dwellings")

all <- left_join(population, elec)
all <- left_join(all, gas)
all <- left_join(all, mot)
all <- left_join(all, experian)
all <- left_join(all, emissions)
all <- left_join(all, heating)

all$pop2016 <- as.numeric(all$pop2016)
all$pop2011 <- as.numeric(all$pop2011)
all$gas_total <- as.numeric(all$gas_total)
all$elec_total <- as.numeric(all$elec_total)
all$median_household_income <- as.numeric(all$median_household_income)

all$gas_percap <- all$gas_total / all$pop2016
all$elec_percap <- all$elec_total / all$pop2016

# Calcualte non-gas heating
median_gas <- median(all$gas_total, na.rm = TRUE)

all$nongas_total <- all$non_gas_dwellings / all$total_dwellings * median_gas
all$nongas_percap <- all$nongas_total / all$pop2016


all$diesel_n[is.na(all$diesel_n)] <- 0
all$`electric diesel_n`[is.na(all$`electric diesel_n`)] <- 0
all$`hybrid electric_n`[is.na(all$`hybrid electric_n`)] <- 0
all$`other_n`[is.na(all$`other_n`)] <- 0
all$petrol_n[is.na(all$petrol_n)] <- 0
all$all_cars_n[is.na(all$all_cars_n)] <- 0
all$median_household_income[is.na(all$median_household_income)] <- median(all$median_household_income, na.rm = TRUE)


all$petrol_emissions <- all$cars_miles * 1.60934 * (1 - all$pmiles_diesel) * all$petrol_co2 /1000 
all$diesel_emissions <- all$cars_miles * 1.60934 * (all$pmiles_diesel) * all$diesel_co2 / 1000
# Convert from kgco2 to litres of fuel
all$petrol_litres <- all$petrol_emissions / 2.18943
all$diesel_litres <- all$diesel_emissions / 2.58935
# Convert to kWh
all$petrol_kwh <- all$petrol_litres * 9.42
all$diesel_kwh <- all$petrol_litres * 10.89
# Total
all$driving_kwh <- all$petrol_kwh + all$diesel_kwh
all$driving_kwh_percap <- all$driving_kwh / all$pop2011




#all <- all[,c("LSOA11","pop2016","median_household_income","driving_kwh_percap","gas_percap","elec_percap","nongas_percap")]


all$income_quantile <- as.integer(cut(all$median_household_income, quantile(all$median_household_income, probs=seq(0,1,0.1)), include.lowest=TRUE))

# from melena person fligher per household per year 2010-18 average
flights <- data.frame(income = seq(1,10,1),
                      flights = c(0.36,0.37,0.52,0.64,0.76,0.94,1.16,1.41,1.80,2.66))


#flights$flight_km_percap <- flights$flights / 100 * 5556  # single flight to New York
#flights$flight_kwh_percap <- flights$flight_km_percap * 0.53 #https://www.withouthotair.com/c5/page_36.shtml

all <- left_join(all, flights, by = c("income_quantile" = "income"))

all$total_flights <- all$flights * all$total_dwellings
all$flight_kwh_percap <- all$total_flights * 5556 * 0.53  / all$pop2016 # flight to new york

summary(all$flight_kwh_percap[all$income_quantile == 10])
#all$flight_kwh_percap2 <- all$flight_kwh_percap + runif(nrow(all), -100, 100) # Jiggle for plotting
#summary(all$flight_kwh_percap2[all$income_quantile == 10])



all$driving_kwh_percap[is.na(all$driving_kwh_percap)] <- 0
all$gas_percap[is.na(all$gas_percap)] <- 0



all_box <- all[,c("driving_kwh_percap","gas_percap","elec_percap","flight_kwh_percap","income_quantile","nongas_percap")]
names(all_box) = c("Driving","Gas","Electricity","Flying","Income","Other Heating")
all_box <- pivot_longer(all_box, cols = c("Electricity","Gas","Driving","Flying","Other Heating")) 
all_box$Income <- factor(all_box$Income, levels = seq(1,10,1))



ggplot(all_box, aes(Income, value, fill = name)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 11000), expand = c(0, 0)) +
  labs(fill = "Energy Type") +
  theme(legend.position = c(0.1, 0.8), 
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  labs(x = "Household Income Decile",
       y = "Annual Energy Consumption (kWh per capita)") +
  ggsave("plots/briefing_note_boxplot.png")
 

# Convert to emissions

all$gas_emission_percap <- all$gas_percap * 0.20428
all$driving_emission_percap <- all$driving_kwh_percap * 0.25
all$elec_emission_percap <- all$elec_percap * 0.2556
all$flight_emission_percap <- all$flight_kwh_percap * 0.281
all$nongas_emission_percap <- all$nongas_percap * 0.25974 # buring oil

all$total_emissions_percap <- rowSums(all[,c("gas_emission_percap","driving_emission_percap","elec_emission_percap","flight_emission_percap","nongas_emission_percap")])

all_box <- all[,c("total_emissions_percap","income_quantile")]
names(all_box) = c("emissions","Income")
all_box <- pivot_longer(all_box, cols = c("emissions")) 
all_box$Income <- factor(all_box$Income, levels = seq(1,10,1))

ggplot(all_box, aes(Income, value, fill = name)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 6000), expand = c(0, 0)) +
  theme(legend.position = "none") +
  labs(x = "Household Income Decile",
       y = "Selected Annual Emissions (CO2 per Capita)") +
  ggsave("plots/briefin_note_co2_box_plot.png")



# Make a map
all$total_percap <- all$driving_kwh_percap + all$gas_percap + all$elec_percap + all$flight_kwh_percap + all$nongas_percap

all_sf <- left_join(bounds, all)
all_sf <- all_sf[!is.na(all_sf$income_quantile),]

tm <- tm_shape(all_sf) +
  tm_fill("total_percap", 
          style = "fixed", 
          palette = "-RdYlBu",
          midpoint = median(all_sf$total_percap),
          breaks = quantile(all_sf$total_percap, probs = seq(0,1,0.1)),
          title = "Selected Annual Energy Use (kWh per capita)") +
  tm_shape(la) +
    tm_borders()

tmap_save(tm, filename = "plots/briefing_note_map.png")
          #breaks = quantile(all_sf$total_percap, probs = seq(0,1,0.1)))

tm2 <- tm_shape(all_sf) +
  tm_fill("total_percap", 
          style = "fixed", 
          palette = "-RdYlBu",
          midpoint = median(all_sf$total_percap),
          breaks = c(1000,5000,8000,10000,12000,14000,16000,18000,20000,30000,41000),
          title = "Selected Annual Energy Use (kWh per capita)",
          auto.palette.mapping=FALSE) +
  tm_shape(la) +
  tm_borders()

tmap_save(tm2, filename = "plots/briefing_note_map_alt.png")
