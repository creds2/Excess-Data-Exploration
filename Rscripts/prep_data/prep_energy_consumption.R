# Prep energy consumption
library(sf)
library(dplyr)

# 2017
lsoa_17 <- read.csv("data-input/Energy Consumption/LSOA_domestic_electricity_2017.csv.csv", stringsAsFactors = F)
lsoa_17 <- lsoa_17[,1:10]
names(lsoa_17) <- as.character(lsoa_17[1,])
lsoa_17 <- lsoa_17[2:nrow(lsoa_17),]
lsoa_17 <- lsoa_17[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Total domestic electricity consumption (kWh)",
                      "Total number of domestic electricity meters",
                      "Mean domestic electricity consumption \n(kWh per meter)",
                      "Median domestic electricity consumption \n(kWh per meter)")]
names(lsoa_17) <- c("LSOA11","TotDomElec_17_kWh","DomMet_17","MeanDomElec_17_kWh","MedianDomElec_17_kWh")
lsoa_17[,1:5] <- lapply(lsoa_17[,1:5], function(x){gsub(" ","",x)})
lsoa_17[,2:5] <- lapply(lsoa_17[,2:5], function(x){gsub(",","",x)})
lsoa_17[,2:5] <- lapply(lsoa_17[,2:5], as.numeric)

# 2016
lsoa_16 <- readxl::read_xlsx("data-input/Energy Consumption/LSOA_domestic_electricity_2016.xlsx", 
                             sheet = "LSOA Dom Elec 2016", col_names = FALSE)
names(lsoa_16) <- as.character(lsoa_16[2,])
lsoa_16 <- lsoa_16[3:nrow(lsoa_16),]
lsoa_16 <- lsoa_16[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Total domestic electricity consumption (kWh)",
                      "Total number of domestic electricity meters",
                      "Mean domestic electricity consumption \r\n(kWh per meter)",
                      "Median domestic electricity consumption \r\n(kWh per meter)")]

names(lsoa_16) <- c("LSOA11","TotDomElec_16_kWh","DomMet_16","MeanDomElec_16_kWh","MedianDomElec_16_kWh")
lsoa_16[,2:5] <- lapply(lsoa_16[,2:5], as.numeric)

# 2015
lsoa_15 <- readxl::read_xlsx("data-input/Energy Consumption/LSOA_domestic_electricity_2015.xlsx", 
                             sheet = "LSOA Dom Elec 2015", col_names = FALSE)
names(lsoa_15) <- as.character(lsoa_15[2,])
lsoa_15 <- lsoa_15[3:nrow(lsoa_15),]
lsoa_15 <- lsoa_15[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Total domestic electricity consumption (kWh)",
                      "Total number of domestic electricity meters",
                      "Mean domestic electricity consumption \r\n(kWh per meter)",
                      "Median domestic electricity consumption \r\n(kWh per meter)")]

names(lsoa_15) <- c("LSOA11","TotDomElec_15_kWh","DomMet_15","MeanDomElec_15_kWh","MedianDomElec_15_kWh")
lsoa_15[,2:5] <- lapply(lsoa_15[,2:5], as.numeric)

# 2014
lsoa_14 <- read.csv("data-input/Energy Consumption/LSOA_domestic_electricity_2014.csv.csv", stringsAsFactors = F)
lsoa_14 <- lsoa_14[,1:10]
names(lsoa_14) <- as.character(lsoa_14[1,])
lsoa_14 <- lsoa_14[2:nrow(lsoa_14),]
lsoa_14 <- lsoa_14[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Total domestic electricity consumption (kWh)",
                      "Total number of domestic electricity meters",
                      "Mean domestic electricity consumption \n(kWh per meter)",
                      "Median domestic electricity consumption \n(kWh per meter)")]
names(lsoa_14) <- c("LSOA11","TotDomElec_14_kWh","DomMet_14","MeanDomElec_14_kWh","MedianDomElec_14_kWh")
lsoa_14[,1:5] <- lapply(lsoa_14[,1:5], function(x){gsub(" ","",x)})
lsoa_14[,2:5] <- lapply(lsoa_14[,2:5], function(x){gsub(",","",x)})
lsoa_14[,2:5] <- lapply(lsoa_14[,2:5], as.numeric)

# 2013
dir.create("tmp")
unzip("data-input/Energy Consumption/2013.zip", exdir = "tmp")
lsoa_13 <- read.csv("tmp/2013/LSOA_domestic_electricity_estimates_2013.csv", stringsAsFactors = F)
lsoa_13 <- lsoa_13[,c("Lower.Layer.Super.Output.Area..LSOA..Code",
                      "Total.domestic.electricity.consumption..kWh.",
                      "Total.number.of.domestic.electricity.meters",
                      "Mean.domestic.electricity.consumption...kWh.per.meter.",
                      "Median.domestic.electricity.consumption...kWh.per.meter.")]
names(lsoa_13) <- c("LSOA11","TotDomElec_13_kWh","DomMet_13","MeanDomElec_13_kWh","MedianDomElec_13_kWh")
lsoa_13[,1:5] <- lapply(lsoa_13[,1:5], function(x){gsub(" ","",x)})
lsoa_13[,2:5] <- lapply(lsoa_13[,2:5], function(x){gsub(",","",x)})
lsoa_13[,2:5] <- lapply(lsoa_13[,2:5], as.numeric)
unlink("tmp", recursive = T)

# 2012
dir.create("tmp")
unzip("data-input/Energy Consumption/2012.zip", exdir = "tmp")
lsoa_12 <- readxl::read_xlsx("tmp/2012/LSOA_domestic_electricity_estimates__2012_.xlsx", 
                             sheet = "LSOA domestic electricity 2012", col_names = FALSE)
names(lsoa_12) <- as.character(lsoa_12[2,])
lsoa_12 <- lsoa_12[3:nrow(lsoa_12),]
lsoa_12 <- lsoa_12[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Total domestic electricity consumption (kWh)",
                      "Total number of domestic electricity meters",
                      "Number of Ordinary Domestic Meters",
                      "Number of Economy 7 Meters",
                      "Average domestic electricity consumption \r\n(kWh per meter)",
                      "Average Ordinary consumption \r\n(kWh per meter)",
                      "Average Economy 7 consumption (kWh per meter)")]
names(lsoa_12) <- c("LSOA11","TotDomElec_12_kWh","DomMet_12","OrdMet_12","Econ7Met_12","MeanDomElec_12_kWh","MeanOrdMetElec_12_kWh","MeanEcon7MetElec_12_kWh")
lsoa_12[,2:8] <- lapply(lsoa_12[,2:8], function(x){gsub(" ","",x)})
lsoa_12[,2:8] <- lapply(lsoa_12[,2:8], function(x){gsub(",","",x)})
lsoa_12[,2:8] <- lapply(lsoa_12[,2:8], as.numeric)
unlink("tmp", recursive = T)

# 2011
dir.create("tmp")
unzip("data-input/Energy Consumption/2011.zip", exdir = "tmp")
lsoa_11 <- readxl::read_xlsx("tmp/2011/LSOA_domestic_electricity_estimates__2011_.xlsx", 
                             sheet = "LSOA domestic electricity 2011", col_names = FALSE)
names(lsoa_11) <- as.character(lsoa_11[2,])
lsoa_11 <- lsoa_11[3:nrow(lsoa_11),]
lsoa_11 <- lsoa_11[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Total domestic electricity consumption (kWh)",
                      "Total number of domestic electricity meters",
                      "Number of Ordinary Domestic Meters",
                      "Number of Economy 7 Meters",
                      "Average domestic electricity consumption \r\n(kWh per meter)",
                      "Average Ordinary consumption \r\n(kWh per meter)",
                      "Average Economy 7 consumption (kWh per meter)")]
names(lsoa_11) <- c("LSOA11","TotDomElec_11_kWh","DomMet_11","OrdMet_11","Econ7Met_11","MeanDomElec_11_kWh","MeanOrdMetElec_11_kWh","MeanEcon7MetElec_11_kWh")
lsoa_11[,2:8] <- lapply(lsoa_11[,2:8], function(x){gsub(" ","",x)})
lsoa_11[,2:8] <- lapply(lsoa_11[,2:8], function(x){gsub(",","",x)})
lsoa_11[,2:8] <- lapply(lsoa_11[,2:8], as.numeric)
unlink("tmp", recursive = T)

# 2010
dir.create("tmp")
unzip("data-input/Energy Consumption/2010.zip", exdir = "tmp")
lsoa_10 <- readxl::read_xls("tmp/2010/4813-llsoa-domestic-elec-est-2010.xls", 
                            sheet = "LLSOA Electricity Domestic", col_names = FALSE)
names(lsoa_10) <- as.character(lsoa_10[1,])
lsoa_10 <- lsoa_10[3:nrow(lsoa_10),]
lsoa_10 <- lsoa_10[,c("Lower Layer Super Output Area (LLSOA) Code",
                      "Ordinary Domestic Consumption (kWh)",
                      "Economy 7 Consumption (kWh)",
                      "Number of Ordinary Domestic Meters",
                      "Number of Economy 7 Meters",
                      "Average Ordinary Domestic Consumption (kWh)",
                      "Average Economy 7 Consumption (kWh)")]
names(lsoa_10) <- c("LSOA11","OrdDomElec_10_kWh","Econ7DomElec_10_kWh","OrdMet_10","Econ7Met_10","MeanOrdMetElec_10_kWh","MeanEcon7MetElec_10_kWh")
lsoa_10[,2:7] <- lapply(lsoa_10[,2:7], function(x){gsub(" ","",x)})
lsoa_10[,2:7] <- lapply(lsoa_10[,2:7], function(x){gsub(",","",x)})
lsoa_10[,2:7] <- lapply(lsoa_10[,2:7], as.numeric)
unlink("tmp", recursive = T)

lsoa_10$DomMet_10 <- rowSums(lsoa_10[,c("OrdMet_10", "Econ7Met_10")], na.rm=TRUE)
lsoa_10$TotDomElec_10_kWh <- rowSums(lsoa_10[,c("OrdDomElec_10_kWh","Econ7DomElec_10_kWh")], na.rm=TRUE)
lsoa_10$MeanDomElec_10_kWh <- lsoa_10$TotDomElec_10_kWh / lsoa_10$DomMet_10

# Link Together
lsoa <- sf::st_read("data-prepared/LSOA_generalised.gpkg")
lsoa$LSOA11 <- as.character(lsoa$LSOA11)

lsoa <- left_join(lsoa, lsoa_17, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_16, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_15, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_14, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_13, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_12, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_11, by = "LSOA11")
lsoa <- left_join(lsoa, lsoa_10, by = "LSOA11")

lsoa2 <- lsoa[,c(1,order(names(lsoa)[2:(ncol(lsoa)-1)]) + 1,ncol(lsoa))]
