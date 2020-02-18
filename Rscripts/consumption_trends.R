# Hisotrical Tends in Gas and Electrict Use
library(reshape2)
library(ggplot2)
library(dplyr)
library(tmap)

lsoa_gas <- read.csv("data-prepared/Gas_2010-17.csv")

lsoa_gas_meters <- lsoa_gas[,c(1,2:9)]
names(lsoa_gas_meters) <- c("LSOA11",as.character(2010:2017))

lsoa_gas_meters[2:9] <- lapply(lsoa_gas_meters[2:9], function(x){x / lsoa_gas_meters$`2017`})

lsoa_gas_meters_melt <- melt(lsoa_gas_meters)
lsoa_gas_meters_melt$variable <- as.integer(as.character(lsoa_gas_meters_melt$variable))
lsoa_gas_meters_melt$group_all <- 1
ggplot(lsoa_gas_meters_melt, 
       aes(x = variable, 
           y = value, 
           group = LSOA11)) +
  geom_line(color = "light blue", alpha = 0.5, size = 1) +
  ylim(0,3)+
  stat_summary(aes(x = variable, 
                   y = value,
                   group = group_all), fun.y=mean, geom="line", colour="red") +
  xlab("Year") +
  ylab("Number of Meters / Number of Meters in 2017") +
  ggsave("plots/energy_trends/gas_meters.jpg")

lsoa_gas_ave <- lsoa_gas[,c(1,10:17)]
names(lsoa_gas_ave) <- c("LSOA11",as.character(2010:2017))

lsoa_gas_ave_melt <- melt(lsoa_gas_ave)
lsoa_gas_ave_melt$variable <- as.integer(as.character(lsoa_gas_ave_melt$variable))
lsoa_gas_ave_melt$group_all <- 1
ggplot(lsoa_gas_ave_melt, 
       aes(x = variable, 
           y = value, 
           group = LSOA11)) +
  geom_line(color = "light blue", alpha = 0.5, size = 1) +
  stat_summary(aes(x = variable, 
                   y = value,
                   group = group_all), fun.y=mean, geom="line", colour="red") +
  xlab("Year") +
  ylab("Mean Domestic Gas Consumption kWh") +
  ggsave("plots/energy_trends/gas_average.jpg")

# Map change

bounds <- sf::read_sf("data-prepared/LSOA_forplots.gpkg")
bounds <- dplyr::left_join(bounds, lsoa_gas_ave)
bounds <- bounds[,c("LSOA11","2013","2017")]
bounds$pchange = (bounds$`2017` -  bounds$`2013`) / bounds$`2013` * 100

map <- tm_shape(bounds) +  # Build the map
  tm_fill("pchange",
          breaks = c(-100,-10,-5,-1,0,1,5,10,110),
          palette = "-RdYlBu", 
          legend.title = "% change in gas consumption")
tmap_save(map, filename = "plots/energy_trends/gas_change.png", dpi = 600)

map <- tm_shape(bounds) +  # Build the map
  tm_fill("2017",
          breaks = quantile(bounds$`2017`, probs = seq(0,1,0.1), na.rm = TRUE),
          palette = "-RdYlBu", 
          legend.title = "Average gas consumption 2017")
tmap_save(map, filename = "plots/energy_trends/gas_2017.png", dpi = 600)


# Electricity

lsoa_elec <- read.csv("data-prepared/Electricty_2010-17.csv")

lsoa_elec_meters <- lsoa_elec[,c(1,2:9)]
names(lsoa_elec_meters) <- c("LSOA11",as.character(2010:2017))

lsoa_elec_meters[2:9] <- lapply(lsoa_elec_meters[2:9], function(x){x / lsoa_elec_meters$`2017`})

lsoa_elec_meters_melt <- melt(lsoa_elec_meters)
lsoa_elec_meters_melt$variable <- as.integer(as.character(lsoa_elec_meters_melt$variable))
lsoa_elec_meters_melt$group_all <- 1
ggplot(lsoa_elec_meters_melt, 
       aes(x = variable, 
           y = value, 
           group = LSOA11)) +
  geom_line(color = "light blue", alpha = 0.5, size = 1) +
  ylim(0,3)+
  stat_summary(aes(x = variable, 
                   y = value,
                   group = group_all), fun.y=mean, geom="line", colour="red") +
  xlab("Year") +
  ylab("Number of Meters / Number of Meters in 2017") +
  ggsave("plots/energy_trends/electric_meters.jpg")

lsoa_elec_ave <- lsoa_elec[,c(1,14:21)]
names(lsoa_elec_ave) <- c("LSOA11",as.character(2010:2017))

lsoa_elec_ave_melt <- melt(lsoa_elec_ave)
lsoa_elec_ave_melt$variable <- as.integer(as.character(lsoa_elec_ave_melt$variable))
lsoa_elec_ave_melt$group_all <- 1
ggplot(lsoa_elec_ave_melt, 
       aes(x = variable, 
           y = value, 
           group = LSOA11)) +
  geom_line(color = "light blue", alpha = 0.5, size = 1) +
  stat_summary(aes(x = variable, 
                   y = value,
                   group = group_all), fun.y=mean, geom="line", colour="red") +
  xlab("Year") +
  ylab("Mean Domestic elec Consumption kWh") +
  ggsave("plots/energy_trends/elec_average.jpg")

# Map change

bounds <- sf::read_sf("data-prepared/LSOA_forplots.gpkg")
bounds <- dplyr::left_join(bounds, lsoa_elec_ave)
bounds <- bounds[,c("LSOA11","2013","2017")]
bounds$pchange = (bounds$`2017` -  bounds$`2013`) / bounds$`2013` * 100

map <- tm_shape(bounds) +  # Build the map
  tm_fill("pchange",
          breaks = c(-100,-10,-5,-1,0,1,5,10,110),
          palette = "-RdYlBu", 
          legend.title = "% change in elec consumption")
tmap_save(map, filename = "plots/energy_trends/elec_change.png", dpi = 600)

map <- tm_shape(bounds) +  # Build the map
  tm_fill("2017",
          breaks = quantile(bounds$`2017`, probs = seq(0,1,0.1), na.rm = TRUE),
          palette = "-RdYlBu", 
          legend.title = "Average electric consumption 2017")
tmap_save(map, filename = "plots/energy_trends/electric_2017.png", dpi = 600)
