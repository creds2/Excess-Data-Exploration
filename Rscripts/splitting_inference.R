# generate data -----------------------------------------------
library(sf)
library(dplyr)
library(Hmisc)
secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/Tim Share"
bounds <- st_read("data-prepared/LSOA_generalised.gpkg")
elec <- read.csv("data-prepared/Electricty_2010-17.csv", stringsAsFactors = FALSE)
elec <- elec[!duplicated(elec$LSOA11),] 
gas <- read.csv("data-prepared/Gas_2010-17.csv", stringsAsFactors = FALSE)
mot <- read.csv(paste0(secure_path,"/From Tim/MOT Data RACv9.3/MOT Data RACv9.3 LSOAoutputs_2011.csv"), stringsAsFactors = FALSE)
age <- readRDS("data-prepared/age.Rds") # Not full UK
census <- readRDS("data-prepared/census_lsoa.Rds") # Not full UK
heating <- readRDS("data-prepared/central_heating.Rds") # Not full UK
EPC <- readRDS("data-prepared/EPC.Rds")
population <- readRDS("data-prepared/population.Rds") # Not full UK
density <- readRDS("data-prepared/populationDensity.Rds")
rooms <- readRDS("data-prepared/rooms.Rds")

dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
tim <- read.csv("temp/XSExpData1.csv", stringsAsFactors = FALSE)
unlink("temp", recursive = T)

dir.create("temp")
unzip("../Secure-Data/Excess/Experian.zip", exdir = "temp")
experian <- read.csv("temp/UKDA-5738-csv/csv/2011-experian-data.csv", stringsAsFactors = FALSE)
unlink("temp", recursive = T)
names(experian) <- experian[4,]
experian <- experian[5:nrow(experian),]
experian <- experian[,c("GeographyValue","Median_(H) Household Income Value")]
names(experian) <- c("LSOA","median_household_income")
emissions <- readRDS("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.Rds")
latitude <- st_centroid(bounds)
latitude <- cbind(st_drop_geometry(latitude), st_coordinates(latitude))
latitude <- latitude[,c("LSOA11","Y")]
names(latitude) <- c("LSOA11","northing")

bounds <- bounds[bounds$LSOA11 %in% census$CODE,]
elec <- elec[elec$LSOA11 %in% bounds$LSOA11, c("LSOA11","DomMet_11",
                                               "DomMet_17","MeanDomElec_11_kWh",
                                               "MeanDomElec_17_kWh",
                                               "TotDomElec_11_kWh","TotDomElec_17_kWh")]
gas <- gas[gas$LSOA11 %in% bounds$LSOA11, c("LSOA11","GasMet_11", "GasMet_17",
                                            "MeanDomGas_11_kWh","MeanDomGas_17_kWh",
                                            "TotDomGas_11_kWh", "TotDomGas_17_kWh")]
mot <- mot[mot$LSOA %in% bounds$LSOA11,]
names(mot) <- c("LSOA", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                "pmiles_car","pmiles_vans","cars_percap","miles_percap")
age <- age[age$lsoa %in% bounds$LSOA11, c("lsoa","BP_PRE_1900","BP_1900_1918","BP_1919_1929",
                                          "BP_1930_1939","BP_1945_1954","BP_1955_1964",
                                          "BP_1965_1972","BP_1973_1982","BP_1983_1992",
                                          "BP_1993_1999","BP_2000_2009","BP_2010_2015",
                                          "BP_UNKNOWN","ALL_PROPERTIES")]
census <- census[census$CODE %in% bounds$LSOA11,]
heating <- heating[heating$LSOA11 %in% bounds$LSOA11,]
EPC <- EPC[EPC$LSOA11 %in% bounds$LSOA11, c("LSOA11","Dwellings","Crr_EE","Ptn_EE")]
population <- population[population$LSOA11 %in% bounds$LSOA11,]
population$pop2011 <- as.numeric(population$pop2011)
population$pop2016 <- as.numeric(population$pop2016)
density$dense_2017 <- as.numeric(density$dense_2017)
density <- density[density$LSOA11 %in% bounds$LSOA11, ]



# Some values are more meaningful as percentages or averages
age$mean_house_age <- sapply(1:nrow(age), function(i){
  sub <- age[i,2:13]
  sub <- as.numeric(sub)
  res <- weighted.mean(x = c(1850,1909,1924,1934.5,1949.5,1959.5,1968.5,
                             1977.5,1987.5,1996,2004.5,2012.5) ,w = sub)
  return(res)
})

age$pP1900  <- round(age$BP_PRE_1900 / age$ALL_PROPERTIES * 100,2)
age$p1900_18 <- round(age$BP_1900_1918 / age$ALL_PROPERTIES * 100,2)
age$p1919_29 <- round(age$BP_1919_1929 / age$ALL_PROPERTIES * 100,2)
age$p1930_39 <- round(age$BP_1930_1939 / age$ALL_PROPERTIES * 100,2)
age$p1945_54 <- round(age$BP_1945_1954 / age$ALL_PROPERTIES * 100,2)
age$p1955_64 <- round(age$BP_1955_1964 / age$ALL_PROPERTIES * 100,2)
age$p1965_72 <- round(age$BP_1965_1972 / age$ALL_PROPERTIES * 100,2)
age$p1973_82 <- round(age$BP_1973_1982 / age$ALL_PROPERTIES * 100,2)
age$p1983_92 <- round(age$BP_1983_1992 / age$ALL_PROPERTIES * 100,2)
age$p1993_99 <- round(age$BP_1993_1999 / age$ALL_PROPERTIES * 100,2)
age$p2000_09 <- round(age$BP_2000_2009 / age$ALL_PROPERTIES * 100,2)
age$p2010_15 <- round(age$BP_2010_2015 / age$ALL_PROPERTIES * 100,2)
age$pUNKNOWN   <- round(age$BP_UNKNOWN / age$ALL_PROPERTIES * 100,2)

age <- age[,c("lsoa","mean_house_age","pP1900","p1900_18","p1919_29",
              "p1930_39","p1945_54","p1955_64","p1965_72","p1973_82",
              "p1983_92","p1993_99","p2000_09","p2010_15","pUNKNOWN")]

heating$pHeating_None <- round(heating$`No CH` / heating$All * 100,2)
heating$pHeating_Gas <- round(heating$Gas / heating$All * 100,2)
heating$pHeating_Electric <- round(heating$Electric / heating$All * 100,2)
heating$pHeating_Other <- round((heating$Oil + heating$`Solid fuel` + heating$Other)/ heating$All * 100,2)

heating <- heating[,c("LSOA11","pHeating_None","pHeating_Gas","pHeating_Electric","pHeating_Other")]

# Join Togther
all <- left_join(elec, gas, by = "LSOA11")
all <- left_join(all, mot, by = c("LSOA11" = "LSOA"))
all <- left_join(all, age, by = c("LSOA11" = "lsoa"))
all <- left_join(all, census, by = c("LSOA11" = "CODE"))
all <- left_join(all, heating, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, EPC, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, population, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, density, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, experian, by = c("LSOA11" = "LSOA"))
all <- left_join(all, rooms, by = c("LSOA11" = "geography.code"))
all <- left_join(all, emissions, by = c("LSOA11" = "LSOA"))
all <- left_join(all, latitude, by = c("LSOA11" = "LSOA11"))
rm(age, census, density, elec, EPC, gas, heating, mot, population, experian, rooms, emissions,latitude)

all$median_household_income <- as.numeric(all$median_household_income)
# Get average car emissions, by weighting miles by emissions

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

all_noNA <- all[!is.na(all$MeanDomGas_11_kWh), ]
all_noNA <- all_noNA[,!sapply(all_noNA, anyNA)]
all_noNA <- all_noNA[,!names(all_noNA) %in% c("DomMet_17","MeanDomElec_17_kWh","TotDomElec_17_kWh","GasMet_11",
                                              "TotDomGas_11_kWh","cars_total","cars_miles","pu5k",
                                              "TotDomGas_11_kWh","GasMet_11","MeanDomElec_17_kWh")]

X <- data.matrix(all_noNA[,!names(all_noNA) %in% c("LSOA11","MeanDomGas_11_kWh")])

# I generate a Y variabile which is a linear combination of these 5 variables
Y <- all_noNA$MeanDomGas_11_kWh

# obviously the lm - estimates are super good
coefficients(lm(Y ~ ., data = data.frame(X)))

# sample splitting inference ----------------------------------------------

# Split data
indexes <- sample(1:nrow(X), size = nrow(X) / 2)

# indentify important variable using only train data
# YOU SHOULD DO THIS USING THE TREE ALGORITHM
lm_model <- lm(Y ~ ., data = data.frame(X), subset = indexes) 
signif_variables <- names(which(summary(lm_model)$coefficients[, 4] < 0.05))

# Estimate coef of these "important" variables using the other part of data
estimate_coefs_formula <- paste0("Y ~ ", paste(setdiff(signif_variables, "(Intercept)"), collapse = " + "))
estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = data.frame(X), subset = -indexes)
sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
res <- sample_split_coefs

# Repeat the same idea again and again
n_replicates <- 1000
for(i in seq_len(n_replicates)) {
  indexes <- sample(1:nrow(X), size = nrow(X) / 2)
  
  lm_model <- lm(Y ~ ., data = data.frame(X), subset = indexes) 
  signif_variables <- names(which(summary(lm_model)$coefficients[, 4] < 0.05))
  
  estimate_coefs_formula <- paste0("Y ~ ", paste(setdiff(signif_variables, "(Intercept)"), collapse = " + "))
  estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = data.frame(X), subset = -indexes)
  
  sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
  colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
  res <- dplyr::bind_rows (res, sample_split_coefs)
  
  if (!i %% 100) print(i)
}

# results


foo <- res %>% 
  summarize_all(function(x) sum(!is.na(x)))
foo <- t(foo)
plot(foo)

summary(foo > 990)
bar <- foo[foo > 990,]

sub <- all_noNA[,names(all_noNA) %in% c("MeanDomGas_11_kWh",names(bar))]

lm1 <- lm(MeanDomGas_11_kWh ~ ., data = sub)
summary(lm1)
