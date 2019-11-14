library(sf)
library(dplyr)
library(Hmisc)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}

bounds <- st_read("data-prepared/LSOA_generalised.gpkg")
elec <- read.csv("data-prepared/Electricty_2010-17.csv", stringsAsFactors = FALSE)
elec <- elec[!duplicated(elec$LSOA11),] 
gas <- read.csv("data-prepared/Gas_2010-17.csv", stringsAsFactors = FALSE)
mot <- read.csv(paste0(secure_path,"/Tim Share/From Tim/MOT Data RACv9.3/MOT Data RACv9.3 LSOAoutputs_2011.csv"), stringsAsFactors = FALSE)
age <- readRDS("data-prepared/age.Rds") # Not full UK
census <- readRDS("data-prepared/census_lsoa.Rds") # Not full UK
country <- readRDS("data-prepared/country_birth.Rds") # EW only
names(census) <- gsub("[.]","",names(census))

heating <- readRDS("data-prepared/central_heating.Rds") # Not full UK
EPC <- readRDS("data-prepared/EPC.Rds")
population <- readRDS("data-prepared/population.Rds") # Not full UK
density <- readRDS("data-prepared/populationDensity.Rds")
rooms <- readRDS("data-prepared/rooms.Rds")
ru <- readRDS("data-prepared/ruralurban.Rds") #EW only

access_employ <- readRDS("data-prepared/access_employ.Rds")
access_food <- readRDS("data-prepared/access_food.Rds")
access_gp <- readRDS("data-prepared/access_gp.Rds")
access_primary <- readRDS("data-prepared/access_primary.Rds")
access_secondary <- readRDS("data-prepared/access_secondary.Rds")
access_town <- readRDS("data-prepared/access_town.Rds")

# dir.create("temp")
# unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
# tim <- read.csv("temp/XSExpData1.csv", stringsAsFactors = FALSE)
# unlink("temp", recursive = T)

dir.create("temp")
unzip("../Secure-Data/Excess/Experian.zip", exdir = "temp")
experian <- read.csv("temp/UKDA-5738-csv/csv/2011-experian-data.csv", stringsAsFactors = FALSE)
unlink("temp", recursive = T)
names(experian) <- experian[4,]
experian <- experian[5:nrow(experian),]
experian <- experian[,c("GeographyValue","Median_(H) Household Income Value")]
names(experian) <- c("LSOA","median_household_income")
emissions <- readRDS(paste0(secure_path,"/github-secure-data/lsoa_emissions.Rds"))
latitude <- st_centroid(bounds)
latitude <- cbind(st_drop_geometry(latitude), st_coordinates(latitude))
latitude <- latitude[,c("LSOA11","Y")]
names(latitude) <- c("LSOA11","northing")

bounds <- bounds[bounds$LSOA11 %in% census$CODE,]
elec <- elec[elec$LSOA11 %in% bounds$LSOA11, c("LSOA11",
                                               #"DomMet_11","DomMet_17",
                                               "MeanDomElec_11_kWh",#"MeanDomElec_17_kWh"#,
                                               "TotDomElec_11_kWh"#,"TotDomElec_17_kWh"
                                               )]
gas <- gas[gas$LSOA11 %in% bounds$LSOA11, c("LSOA11",
                                            #"GasMet_11", "GasMet_17",
                                            "MeanDomGas_11_kWh",#"MeanDomGas_17_kWh"#,
                                            "TotDomGas_11_kWh"#, "TotDomGas_17_kWh"
                                            )]
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
all <- left_join(all, ru, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, country, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, access_employ, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, access_food, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, access_gp, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, access_primary, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, access_secondary, by = c("LSOA11" = "LSOA11"))
all <- left_join(all, access_town, by = c("LSOA11" = "LSOA11"))
rm(age, census, density, elec, EPC, gas, heating, mot, population, experian, rooms, emissions,latitude, ru, country)

all$median_household_income <- as.numeric(all$median_household_income)



# Get average car emissions, by weighting miles by emissions

# fix NAs
all$diesel_n[is.na(all$diesel_n)] <- 0
all$`electric diesel_n`[is.na(all$`electric diesel_n`)] <- 0
all$`hybrid electric_n`[is.na(all$`hybrid electric_n`)] <- 0
all$`other_n`[is.na(all$`other_n`)] <- 0
all$petrol_n[is.na(all$petrol_n)] <- 0
all$all_cars_n[is.na(all$all_cars_n)] <- 0
all$median_household_income[is.na(all$median_household_income)] <- median(all$median_household_income, na.rm = TRUE)

# missing dtf stats

# is_england <- substr(all$LSOA11,1,1)
# is_england <- is_england == "E"
# 
# all$acc_employ_PTtime[is.na(all$acc_employ_PTtime ) & is_england] <- 0
# all$acc_employ_PTfrequency[is.na(all$acc_employ_PTfrequency ) & is_england] <- 0
# all$acc_employ_cycletime[is.na(all$acc_employ_cycletime ) & is_england] <- 0
# all$acc_employ_cartime[is.na(all$acc_employ_cartime ) & is_england] <- 0          
# all$acc_employ_p20min_PT_walk[is.na(all$acc_employ_p20min_PT_walk ) & is_england] <- 100
# all$acc_employ_p20min_cycle[is.na(all$acc_employ_p20min_cycle ) & is_england] <- 100
# all$acc_employ_p20min_car[is.na(all$acc_employ_p20min_car ) & is_england] <- 100
# all$acc_employ_p20min_composite[is.na(all$acc_employ_p20min_composite ) & is_england] <- 100 
# all$acc_employ_p40min_PT_walk[is.na(all$acc_employ_p40min_PT_walk ) & is_england] <- 100
# all$acc_employ_p40min_cycle[is.na(all$acc_employ_p40min_cycle ) & is_england] <- 100
# all$acc_employ_p40min_car[is.na(all$acc_employ_p40min_car ) & is_england] <- 100
# all$acc_employ_p40min_composite[is.na(all$acc_employ_p40min_composite ) & is_england] <- 100 
# 
# all$acc_food_PTtime[is.na(all$acc_food_PTtime ) & is_england] <- 0
# all$acc_food_PTfrequency[is.na(all$acc_food_PTfrequency ) & is_england] <- 0
# all$acc_food_cycletime[is.na(all$acc_food_cycletime ) & is_england] <- 0
# all$acc_food_cartime[is.na(all$acc_food_cartime ) & is_england] <- 0            
# all$acc_food_p15min_PT_walk[is.na(all$acc_food_p15min_PT_walk ) & is_england] <- 100
# all$acc_food_p15min_cycle[is.na(all$acc_food_p15min_cycle ) & is_england] <- 100
# all$acc_food_p15min_car[is.na(all$acc_food_p15min_car ) & is_england] <- 100
# all$acc_food_p15min_composite[is.na(all$acc_food_p15min_composite ) & is_england] <- 100   
# all$acc_food_p30min_PT_walk[is.na(all$acc_food_p30min_PT_walk ) & is_england] <- 100
# all$acc_food_p30min_cycle[is.na(all$acc_food_p30min_cycle ) & is_england] <- 100
# all$acc_food_p30min_car[is.na(all$acc_food_p30min_car ) & is_england] <- 100
# all$acc_food_p30min_composite[is.na(all$acc_food_p30min_composite ) & is_england] <- 100 
# 
# all$acc_gp_PTtime[is.na(all$acc_gp_PTtime ) & is_england] <- 0
# all$acc_gp_PTfrequency[is.na(all$acc_gp_PTfrequency ) & is_england] <- 0
# all$acc_gp_cycletime[is.na(all$acc_gp_cycletime ) & is_england] <- 0
# all$acc_gp_cartime[is.na(all$acc_gp_cartime ) & is_england] <- 0              
# all$acc_gp_p15min_PT_walk[is.na(all$acc_gp_p15min_PT_walk ) & is_england] <- 100
# all$acc_gp_p15min_cycle[is.na(all$acc_gp_p15min_cycle ) & is_england] <- 100
# all$acc_gp_p15min_car[is.na(all$acc_gp_p15min_car ) & is_england] <- 100
# all$acc_gp_p30min_PT_walk[is.na(all$acc_gp_p30min_PT_walk ) & is_england] <- 100       
# all$acc_gp_p30min_cycle[is.na(all$acc_gp_p30min_cycle ) & is_england] <- 100
# all$acc_gp_p30min_car[is.na(all$acc_gp_p30min_car ) & is_england] <- 100
# 
# all$acc_primary_PTtime[is.na(all$acc_primary_PTtime ) & is_england] <- 0
# all$acc_primary_PTfrequency[is.na(all$acc_primary_PTfrequency ) & is_england] <- 0     
# all$acc_primary_cycletime[is.na(all$acc_primary_cycletime ) & is_england] <- 0
# all$acc_primary_cartime[is.na(all$acc_primary_cartime ) & is_england] <- 0
# all$acc_primary_p15min_PT_walk[is.na(all$acc_primary_p15min_PT_walk ) & is_england] <- 100
# all$acc_primary_p15min_cycle[is.na(all$acc_primary_p15min_cycle ) & is_england] <- 100    
# all$acc_primary_p15min_car[is.na(all$acc_primary_p15min_car ) & is_england] <- 100
# all$acc_primary_p30min_PT_walk[is.na(all$acc_primary_p30min_PT_walk ) & is_england] <- 100
# all$acc_primary_p30min_cycle[is.na(all$acc_primary_p30min_cycle ) & is_england] <- 100
# all$acc_primary_p30min_car[is.na(all$acc_primary_p30min_car ) & is_england] <- 100 
# 
# all$acc_secondary_PTtime[is.na(all$acc_secondary_PTtime ) & is_england] <- 0
# all$acc_secondary_PTfrequency[is.na(all$acc_secondary_PTfrequency ) & is_england] <- 0
# all$acc_secondary_cycletime[is.na(all$acc_secondary_cycletime ) & is_england] <- 0
# all$acc_secondary_cartime[is.na(all$acc_secondary_cartime ) & is_england] <- 0       
# all$acc_secondary_p20min_PT_walk[is.na(all$acc_secondary_p20min_PT_walk ) & is_england] <- 100
# all$acc_secondary_p20min_cycle[is.na(all$acc_secondary_p20min_cycle ) & is_england] <- 100
# all$acc_secondary_p20min_car[is.na(all$acc_secondary_p20min_car ) & is_england] <- 100
# all$acc_secondary_p40min_PT_walk[is.na(all$acc_secondary_p40min_PT_walk ) & is_england] <- 100
# all$acc_secondary_p40min_cycle[is.na(all$acc_secondary_p40min_cycle ) & is_england] <- 100
# all$acc_secondary_p40min_car[is.na(all$acc_secondary_p40min_car ) & is_england] <- 100
# 
# all$acc_town_PTtime[is.na(all$acc_town_PTtime ) & is_england] <- 0
# all$acc_town_PTfrequency[is.na(all$acc_town_PTfrequency ) & is_england] <- 0        
# all$acc_town_cycletime[is.na(all$acc_town_cycletime ) & is_england] <- 0
# all$acc_town_cartime[is.na(all$acc_town_cartime ) & is_england] <- 0
# all$acc_town_p15min_PT_walk[is.na(all$acc_town_p15min_PT_walk ) & is_england] <- 100
# all$acc_town_p15min_cycle[is.na(all$acc_town_p15min_cycle ) & is_england] <- 100       
# all$acc_town_p15min_car[is.na(all$acc_town_p15min_car ) & is_england] <- 100
# all$acc_town_p15min_composite[is.na(all$acc_town_p15min_composite ) & is_england] <- 100
# all$acc_town_p30min_PT_walk[is.na(all$acc_town_p30min_PT_walk ) & is_england] <- 100
# all$acc_town_p30min_cycle[is.na(all$acc_town_p30min_cycle ) & is_england] <- 100       
# all$acc_town_p30min_car[is.na(all$acc_town_p30min_car ) & is_england] <- 100
# all$acc_town_p30min_composite[is.na(all$acc_town_p30min_composite ) & is_england] <- 100






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

sapply(all, function(x){sum(is.na(x))})

saveRDS(all,paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))
