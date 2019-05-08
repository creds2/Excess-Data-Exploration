#Do Compile New Census Data File
# Packages and Setup ------------------------------------------------------
library(readr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(xgboost)
library(data.table)

#LSOAs
# Input Data --------------------------------------------------------------
census<-fread("../Downloaded Data/Census Data/Census_Data_143vars_LSOAs.csv")
#headers<-fread("../Downloaded Data/Census Data/Census_Data_188vars_LSOAs.csv")
varnames<-fread("../Downloaded Data/Census Data/ChosenVariableNames.csv")
#---------------------------------------------

#Create Percentages for Travel to Work and Social Grade

TTWnum<-census[,c("CODE","QS701EW0001","QS701EW0002","QS701EW0003","QS701EW0004","QS701EW0005","QS701EW0006","QS701EW0007","QS701EW0008",
"QS701EW0009","QS701EW0010","QS701EW0011","QS701EW0012","QS701EW0013")]

gradenum<-census[,c("CODE","QS611EW0001","QS611EW0002","QS611EW0003","QS611EW0004","QS611EW0005")]

t<-TTWnum[, lapply(.SD, `/`, TTWnum$QS701EW0001), .SDcols = c("QS701EW0002","QS701EW0003","QS701EW0004","QS701EW0005","QS701EW0006","QS701EW0007","QS701EW0008",
                                                            "QS701EW0009","QS701EW0010","QS701EW0011","QS701EW0012","QS701EW0013")]
t<-t*100
TTWperc<-cbind(TTWnum[,1:2],t) 
names(TTWperc)<-c("CODE","All_T2W#","T2W_Home%","T2W_Metro%","T2W_Train%","T2W_Bus%","T2W_Taxi%","T2W_Mbike%","T2W_Car%","T2W_Passenger%","T2W_Cycle%","T2W_Foot%","T2W_Other%","T2W_NoEmp%")


t<-gradenum[, lapply(.SD, `/`, gradenum$QS611EW0001), .SDcols = c("QS611EW0002","QS611EW0003","QS611EW0004","QS611EW0005")]
gradeperc<-cbind(gradenum[,1:2],t) 
names(gradeperc)<-c("CODE","All_Grade#","SocGrade_AB%","SocGrade_C1%","SocGrade_C2%","SocGrade_DE%")


#Select and rename variable.names
vars<-census[, varnames$Variable, with=FALSE]
names(vars)<-varnames$Name

#Join Up Tables
out<-merge(vars,TTWperc,by="CODE")
out<-merge(out,gradeperc,by="CODE")

fwrite(out,"../Processed Data/CensusData_LSOA(86).csv")


#OAs
# Input Data --------------------------------------------------------------
census<-fread("../Downloaded Data/Census Data/Census_Data_143vars_OAs.csv")
headers<-fread("../Downloaded Data/Census Data/Census_Data_188vars_LSOAs.csv")
varnames<-fread("../Downloaded Data/Census Data/ChosenVariableNames.csv")
#---------------------------------------------

#Create Percentages for Travel to Work and Social Grade

TTWnum<-census[,c("CODE","QS701EW0001","QS701EW0002","QS701EW0003","QS701EW0004","QS701EW0005","QS701EW0006","QS701EW0007","QS701EW0008",
                  "QS701EW0009","QS701EW0010","QS701EW0011","QS701EW0012","QS701EW0013")]

gradenum<-census[,c("CODE","QS611EW0001","QS611EW0002","QS611EW0003","QS611EW0004","QS611EW0005")]

t<-TTWnum[, lapply(.SD, `/`, TTWnum$QS701EW0001), .SDcols = c("QS701EW0002","QS701EW0003","QS701EW0004","QS701EW0005","QS701EW0006","QS701EW0007","QS701EW0008",
                                                              "QS701EW0009","QS701EW0010","QS701EW0011","QS701EW0012","QS701EW0013")]
t<-t*100
TTWperc<-cbind(TTWnum[,1:2],t) 
names(TTWperc)<-c("CODE","All_T2W#","T2W_Home%","T2W_Metro%","T2W_Train%","T2W_Bus%","T2W_Taxi%","T2W_Mbike%","T2W_Car%","T2W_Passenger%","T2W_Cycle%","T2W_Foot%","T2W_Other%","T2W_NoEmp%")


t<-gradenum[, lapply(.SD, `/`, gradenum$QS611EW0001), .SDcols = c("QS611EW0002","QS611EW0003","QS611EW0004","QS611EW0005")]
gradeperc<-cbind(gradenum[,1:2],t) 
names(gradeperc)<-c("CODE","All_Grade#","SocGrade_AB%","SocGrade_C1%","SocGrade_C2%","SocGrade_DE%")


#Select and rename variable.names
vars<-census[, varnames$Variable, with=FALSE]
names(vars)<-varnames$Name

#Join Up Tables
out<-merge(vars,TTWperc,by="CODE")
out<-merge(out,gradeperc,by="CODE")

fwrite(out,"../Processed Data/CensusData_OA(86).csv")
