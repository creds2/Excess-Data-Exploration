# Cluster by basic building characteritics
library(dplyr)
library(ggplot2)
library(xgboost)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

census <- readRDS("data-prepared/census_lsoa.Rds")

# join togther

lsoa <- left_join(lsoa,census[,-which(names(lsoa) %in% c("Age_Mean","NoCH"))], by = c("LSOA11CD" = "CODE"))



lsoa_house<-lsoa[,which(names(lsoa) %in% c("LSOA11CD","Region","elecAv","gasAv",
"nrgHH","SupergroupCode","SupergroupName","RU",
"RUC11CD","cluster","Income","IMD",
"fp10","fpLIHC","FullTime","PartTime",
"SelfEmployed","Student","Unemployed","Retired",
"Carers","Sick","Old","Young",
"Age_Mean","Age_Med","AB","C1",
"C2","DE","Outright","Mortgage",
"Social","Private","Single","NoKids",
"Kids","HHsize",
"totalhh",
"Reg","SGN","People_.",
"PopDens","Males.","Females.",
"Live_HH.","Live_Comm.","Age_Mean.y","Age_Median",
"Age0to4.","Age5to7.","Age8to9.","Age10to14.",
"Age15.","Age16to17.","Age18to19.","Age20to24.",
"Age25to29.","Age30to44.","Age45to59.","Age60to64.",
"Age65to74.","Age75to84.","Age85to89.","Age90plus.",
"Unshared.","Shared2.","Shared3plus.","HHwithRes.",
"HHnoRes.",
"Outright.","Mortgage.","Shared.","Social_LA.",
"Social_Other.","Rented_Landlord.","Rented_Other.","RentFree.",
"Occupancy_Rooms.","Occupancy_Bedrooms.",
"PartTime.","FullTime.","Self.Emp.","Unemployed.",
"FTStudent.","Retired.","Student_Inactive.","Carer.",
"Sick.","Inactive_Other.","Unemp_16to24.","Unemp_50to74.",
"Unemp_NeverWorked.","Unemp_LongTerm.","All_Grade.",
"SocGrade_AB.","SocGrade_C1.","SocGrade_C2.","SocGrade_DE.",
"T2W_Home."))]



#Not selected
#Energy
#"all","dom"
#General
#"Area_Hectares",
#Building
#"Detached","SemiDetached","Terraced","Flat","Whole_House_Detached.","Whole_House_Semi.","Whole_House_Terraced",
#"Flat_PurposeBuilt.","Flat_Converted.","Flat_Commercial.","Caravan.","Rooms","NoCH",
#Transport
#"All_T2W.","T2W_Home.","T2W_Metro.","T2W_Train.","T2W_Bus.","T2W_Taxi.",
#"T2W_Mbike.","T2W_Car.","T2W_Passenger.","T2W_Cycle.",
#"T2W_Foot.","T2W_Other.","T2W_NoEmp."
#"NoCarsHH","X1CarHH.","X2CarHH.","X3CarHH.","X4plusCarHH.",
#"hhnocar","hhwithcar","totalcar","percNoCar","carsHHcar","carsHH","ptte",
#"carte","ptttc","carttc","denonbc","denoffbc","denoffrmt","motor","pt","active","home","AvT2W",

lsoa_house$gasAv[is.na(lsoa_house$gasAv)] <- 0

# Look at importnace
lsoa_house_mat <- as.matrix(lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")])
lsoa_house_mat <- lsoa_house_mat[,12:ncol(lsoa_house_mat)]


# Electricity
m_elec = xgboost::xgboost(data = lsoa_house_mat,
                          label = lsoa_house$elecAv, nrounds = 10, max_depth = 5)

importance_m_elec = xgboost::xgb.importance(model = m_elec, feature_names = colnames(lsoa_house_mat))
xgboost::xgb.plot.importance(importance_m_elec, top_n = 15)

# Gas
m_gas = xgboost::xgboost(data = lsoa_house_mat,
                         label = lsoa_house$gasAv, nrounds = 10, max_depth = 5)

importance_m_gas = xgboost::xgb.importance(model = m_gas, feature_names = colnames(lsoa_house_mat))
xgboost::xgb.plot.importance(importance_m_gas, top_n = 15)
