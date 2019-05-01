# Prep energy consumption
library(sf)
library(dplyr)

# 2017
lsoa_17 <- read.csv("data-input/Energy Consumption/LSOA_domestic_gas_2017.csv.csv", stringsAsFactors = F)
lsoa_17 <- lsoa_17[,6:10]
names(lsoa_17) <- as.character(lsoa_17[1,])
lsoa_17 <- lsoa_17[2:nrow(lsoa_17),]
lsoa_17 <- lsoa_17[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Consumption (kWh)",
                      "Number of meters",
                      "Mean consumption (kWh per meter)",
                      "Median consumption (kWh per meter)")]
names(lsoa_17) <- c("LSOA11","TotDomGas_17_kWh","GasMet_17","MeanDomGas_17_kWh","MedianDomGas_17_kWh")
lsoa_17[,1:5] <- lapply(lsoa_17[,1:5], function(x){gsub(" ","",x)})
lsoa_17[,2:5] <- lapply(lsoa_17[,2:5], function(x){gsub(",","",x)})
lsoa_17[,2:5] <- lapply(lsoa_17[,2:5], as.numeric)

# 2016
lsoa_16 <- read.csv("data-input/Energy Consumption/LSOA_domestic_gas_2016.csv.csv", stringsAsFactors = F)
lsoa_16 <- lsoa_16[,6:10]
names(lsoa_16) <- as.character(lsoa_16[1,])
lsoa_16 <- lsoa_16[2:nrow(lsoa_16),]
lsoa_16 <- lsoa_16[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Consumption (kWh)",
                      "Number of meters",
                      "Mean consumption (kWh per meter)",
                      "Median consumption (kWh per meter)")]
names(lsoa_16) <- c("LSOA11","TotDomGas_16_kWh","GasMet_16","MeanDomGas_16_kWh","MedianDomGas_16_kWh")
lsoa_16[,1:5] <- lapply(lsoa_16[,1:5], function(x){gsub(" ","",x)})
lsoa_16[,2:5] <- lapply(lsoa_16[,2:5], function(x){gsub(",","",x)})
lsoa_16[,2:5] <- lapply(lsoa_16[,2:5], as.numeric)




# 2015
lsoa_15 <- readxl::read_xlsx("data-input/Energy Consumption/LSOA_domestic_gas_2015.xlsx", 
                             sheet = "LSOA Domestic Gas 2015", col_names = FALSE)
names(lsoa_15) <- as.character(lsoa_15[2,])
lsoa_15 <- lsoa_15[3:nrow(lsoa_15),]
lsoa_15 <- lsoa_15[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Consumption (kWh)",
                      "Number of meters",
                      "Mean consumption (kWh per meter)",
                      "Median consumption (kWh per meter)")]
names(lsoa_15) <- c("LSOA11","TotDomGas_15_kWh","GasMet_15","MeanDomGas_15_kWh","MedianDomGas_15_kWh")
lsoa_15[,2:5] <- lapply(lsoa_15[,2:5], as.numeric)

# 2014
dir.create("tmp")
unzip("data-input/Energy Consumption/LSOA_MSOA_2014_historic_gas_consumption.zip", exdir = "tmp")
lsoa_14 <- read.csv("tmp/LSOA MSOA 2014 historic gas consumption/Lower layer Super Output Area (LSOA) domestic gas estimates 2014 published Jan 2016v2.csv", stringsAsFactors = F)
names(lsoa_14) <- as.character(lsoa_14[1,])
lsoa_14 <- lsoa_14[,6:10]
lsoa_14 <- lsoa_14[2:nrow(lsoa_14),]
lsoa_14 <- lsoa_14[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Consumption (kWh)",
                      "Number of meters",
                      "Mean consumption (kWh per meter)",
                      "Median consumption (kWh per meter)")]
names(lsoa_14) <- c("LSOA11","TotDomGas_14_kWh","GasMet_14","MeanDomGas_14_kWh","MedianDomGas_14_kWh")
lsoa_14[,1:5] <- lapply(lsoa_14[,1:5], function(x){gsub(" ","",x)})
lsoa_14[,2:5] <- lapply(lsoa_14[,2:5], function(x){gsub(",","",x)})
lsoa_14[,2:5] <- lapply(lsoa_14[,2:5], as.numeric)
unlink("tmp", recursive = T)

# 2013
dir.create("tmp")
unzip("data-input/Energy Consumption/2013_gas.zip", exdir = "tmp")
lsoa_13 <- read.csv("tmp/2013/LSOA_domestic_gas_estimates_2013.csv", stringsAsFactors = F)
lsoa_13 <- lsoa_13[,c("LSOA_CODE",
                      "Cons_kWh",
                      "Number_meters",
                      "Mean_cons_kWh",
                      "Median_cons_kWh")]
names(lsoa_13) <- c("LSOA11","TotDomGas_13_kWh","GasMet_13","MeanDomGas_13_kWh","MedianDomGas_13_kWh")
lsoa_13[,1:5] <- lapply(lsoa_13[,1:5], function(x){gsub(" ","",x)})
lsoa_13[,2:5] <- lapply(lsoa_13[,2:5], function(x){gsub(",","",x)})
lsoa_13[,2:5] <- lapply(lsoa_13[,2:5], as.numeric)
unlink("tmp", recursive = T)

# 2012
dir.create("tmp")
unzip("data-input/Energy Consumption/2012_gas.zip", exdir = "tmp")
lsoa_12 <- readxl::read_xlsx("tmp/2012/LSOA_domestic_gas__2012_.xlsx", 
                             sheet = "LSOA domestic gas", col_names = FALSE)
names(lsoa_12) <- as.character(lsoa_12[1,])
lsoa_12 <- lsoa_12[2:nrow(lsoa_12),]
lsoa_12 <- lsoa_12[,c("Lower Layer Super Output Area (LSOA) Code1",
                      "Domestic Consumption (kWh)",
                      "Number of meters",
                      "Average domestic gas consumption \r\n(kWh per meter)")]
names(lsoa_12) <- c("LSOA11","TotDomGas_12_kWh","GasMet_12","MeanDomGas_12_kWh")
lsoa_12[,1:4] <- lapply(lsoa_12[,1:4], function(x){gsub(" ","",x)})
lsoa_12[,2:4] <- lapply(lsoa_12[,2:4], function(x){gsub(",","",x)})
lsoa_12[,2:4] <- lapply(lsoa_12[,2:4], as.numeric)
unlink("tmp", recursive = T)

# 2011
dir.create("tmp")
unzip("data-input/Energy Consumption/2011_gas.zip", exdir = "tmp")
lsoa_11 <- readxl::read_xlsx("tmp/2011/LSOA_domestic_gas__2011_.xlsx", 
                             sheet = "2011 LSOA Domestic Gas", col_names = FALSE)
names(lsoa_11) <- as.character(lsoa_11[2,])
lsoa_11 <- lsoa_11[3:nrow(lsoa_11),]
lsoa_11 <- lsoa_11[,c("Lower Layer Super Output Area (LSOA) Code",
                      "Consumption (kWh)",
                      "Number of meters",
                      "Average consumption (kWh per meter)")]
names(lsoa_11) <- c("LSOA11","TotDomGas_11_kWh","GasMet_11","MeanDomGas_11_kWh")
lsoa_11[,1:4] <- lapply(lsoa_11[,1:4], function(x){gsub(" ","",x)})
lsoa_11[,2:4] <- lapply(lsoa_11[,2:4], function(x){gsub(",","",x)})
lsoa_11[,2:4] <- lapply(lsoa_11[,2:4], as.numeric)
unlink("tmp", recursive = T)

# 2010
dir.create("tmp")
unzip("data-input/Energy Consumption/2010_gas.zip", exdir = "tmp")
lsoa_10 <- readxl::read_xls("tmp/2010/4814-llsoa-domestic-gas-est-2010.xls", 
                             sheet = "LLSOA Domestic gas", col_names = FALSE)
names(lsoa_10) <- as.character(lsoa_10[1,])
lsoa_10 <- lsoa_10[3:nrow(lsoa_10),]
lsoa_10 <- lsoa_10[,c("Lower Layer Super Output Area (LLSOA) Code",
                      "Consumption (kWh)",
                      "Number of Meters",
                      "Average consumption (kWh)")]
names(lsoa_10) <- c("LSOA11","TotDomGas_10_kWh","GasMet_10","MeanDomGas_10_kWh")
lsoa_10[,1:4] <- lapply(lsoa_10[,1:4], function(x){gsub(" ","",x)})
lsoa_10[,2:4] <- lapply(lsoa_10[,2:4], function(x){gsub(",","",x)})
lsoa_10[,2:4] <- lapply(lsoa_10[,2:4], as.numeric)
unlink("tmp", recursive = T)

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

lsoa <- lsoa[,c(1,order(names(lsoa)[2:(ncol(lsoa)-1)]) + 1,ncol(lsoa))]
write_sf(lsoa,"data-prepared/Gas_2010-17.gpkg")
st_geometry(lsoa) <- NULL
write.csv(lsoa,"data-prepared/Gas_2010-17.csv", na = "", row.names = FALSE)
