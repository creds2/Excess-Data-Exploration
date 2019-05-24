# Cluster by basic building characteritics
library(dplyr)
library(ggplot2)
library(xgboost)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- fread("temp/XSExpData1.csv")

unlink("temp", recursive = T)
mot<-fread("../Secure-Data/Excess/MOT Data/2011_LSOA_Energy_and_emissionsv9a.csv")



census <- readRDS("data-prepared/census_lsoa.Rds")

data<-as.data.table(merge(lsoa[,-c("Age_Mean","NoCH")],census,by.x="LSOA11CD",by.y="CODE",all.x=TRUE))
data<-as.data.table(merge(lsoa,lsoa,by.x="LSOA11CD",by.y="CODE",all.x=TRUE))

m<-mot[,.(lsoa,N,kmAv,sizeAv,nrgAv,nrgTot)]
m<-merge(m,data[,c("LSOA11CD","totalhh","hhnocar","hhwithcar")],by.x="lsoa",by.y="LSOA11CD",all.y=TRUE)
m[,nrgHH:=nrgTot/totalhh][,nrgHHcar:=nrgTot/hhwithcar]
m[,pHHnocar:=hhnocar*100/totalhh];m[,pHHcar:=hhwithcar*100/totalhh]



dTrans<-merge(m,data[,c("LSOA11CD","PopDens","All_T2W.","T2W_Home.","T2W_Metro.","T2W_Train.","T2W_Bus.","T2W_Taxi.",
  "T2W_Mbike.","T2W_Car.","T2W_Passenger.","T2W_Cycle.",
  "T2W_Foot.","T2W_Other.","T2W_NoEmp.",
  "NoCarsHH","X1CarHH.","X2CarHH.","X3CarHH.","X4plusCarHH.",
  "totalcar","percNoCar","carsHHcar","carsHH","ptte",
  "carte","ptttc","carttc","denonbc","denoffbc","denoffrmt","motor","pt","active","home","AvT2W")],
  by.x="lsoa",by.y="LSOA11CD",all.y=TRUE)

#take out pop dens and pHHnoCar
dSoc<-merge(m[,-c("pHHnocar","pHHcar")],data[,c("LSOA11CD","Income","IMD","fp10","fpLIHC","FullTime","PartTime",
                      "SelfEmployed","Student","Unemployed","Retired","Carers","Sick","Old","Young",
                      "Age_Mean","Age_Med","AB","C1","C2","DE","Outright","Mortgage",
                      "Social","Private","Single","NoKids","Kids","HHsize","totalhh","Reg","SGN","People_.",
                      "Males.","Females.","Live_HH.","Live_Comm.",
                      "Age0to4.","Age5to7.","Age8to9.","Age10to14.","Age15.","Age16to17.","Age18to19.","Age20to24.",
                      "Age25to29.","Age30to44.","Age45to59.","Age60to64.","Age65to74.","Age75to84.","Age85to89.","Age90plus.",
                      "Unshared.","Shared2.","Shared3plus.","HHwithRes.","HHnoRes.",
                      "Outright.","Mortgage.","Shared.","Social_LA.","Social_Other.","Rented_Landlord.","Rented_Other.","RentFree.",
                      "Occupancy_Rooms.","Occupancy_Bedrooms.","PartTime.","FullTime.","Self.Emp.","Unemployed.",
                      "FTStudent.","Retired.","Student_Inactive.","Carer.","Sick.","Inactive_Other.","Unemp_16to24.","Unemp_50to74.",
                      "Unemp_NeverWorked.","Unemp_LongTerm.","All_Grade.","SocGrade_AB.","SocGrade_C1.","SocGrade_C2.","SocGrade_DE.",
                      "T2W_Home.")],
              by.x="lsoa",by.y="LSOA11CD",all.y=TRUE)


#rm(census);rm(lsoa)

lsoa_car<-as.data.frame(dTrans)

# Look at importnace
lsoa_car_mat <- as.matrix(lsoa_car[,sapply(lsoa_car,class) %in% c("integer","numeric")])
lsoa_car_mat <- lsoa_car_mat[,11:ncol(lsoa_car_mat)]


# nrgHH
m_nrgHH = xgboost::xgboost(data = lsoa_car_mat,
                          label = lsoa_car$nrgHH, nrounds = 10, max_depth = 5)

importance_m_nrgHH = xgboost::xgb.importance(model = m_nrgHH, feature_names = colnames(lsoa_car_mat))
xgboost::xgb.plot.importance(importance_m_nrgHH, top_n = 15)

#
# nrgHHcar
m_nrgHHcar = xgboost::xgboost(data = lsoa_car_mat,
                           label = lsoa_car$nrgHHcar, nrounds = 10, max_depth = 5)

importance_m_nrgHHcar = xgboost::xgb.importance(model = m_nrgHHcar, feature_names = colnames(lsoa_car_mat))
xgboost::xgb.plot.importance(importance_m_nrgHHcar, top_n = 15)

# Look at social importnace
lsoa_car<-as.data.frame(dSoc)


lsoa_car_mat <- as.matrix(lsoa_car[,sapply(lsoa_car,class) %in% c("integer","numeric")])
lsoa_car_mat <- lsoa_car_mat[,11:ncol(lsoa_car_mat)]


# nrgHH
m_nrgHH = xgboost::xgboost(data = lsoa_car_mat,
                           label = lsoa_car$nrgHH, nrounds = 10, max_depth = 5)

importance_m_nrgHH = xgboost::xgb.importance(model = m_nrgHH, feature_names = colnames(lsoa_car_mat))
xgboost::xgb.plot.importance(importance_m_nrgHH, top_n = 15)

#
# nrgHHcar
m_nrgHHcar = xgboost::xgboost(data = lsoa_car_mat,
                              label = lsoa_car$nrgHHcar, nrounds = 10, max_depth = 5)

importance_m_nrgHHcar = xgboost::xgb.importance(model = m_nrgHHcar, feature_names = colnames(lsoa_car_mat))
xgboost::xgb.plot.importance(importance_m_nrgHHcar, top_n = 15)
