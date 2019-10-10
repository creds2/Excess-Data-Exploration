# testing glmulit
library(MASS)
mydata <- all[,c("MeanDomGas_11_kWh",
                 "mean_house_age",
                 "pP1900","p1900_18","p1919_29","p1930_39","p1945_54",
                 "p1955_64","p1965_72","p1973_82","p1983_92","p1993_99",
                 "p2000_09","p2010_15","pUNKNOWN",
                 "Males.","Females.","Live_HH.","Live_Comm.",
                 "Age0to4.","Age5to7.","Age8to9.",
                 "Age10to14.","Age15.","Age16to17.","Age18to19.","Age20to24.",
                 "Age25to29.","Age30to44.","Age45to59.","Age60to64.","Age65to74.",
                 "Age75to84.","Age85to89.","Age90plus.","Unshared.","Shared2.",
                 "Shared3plus.","HHwithRes.","HHnoRes.","Whole_House_Detached.","Whole_House_Semi.",
                 "Whole_House_Terraced","Flat_PurposeBuilt.","Flat_Converted.","Flat_Commercial.","Caravan.",
                 "Outright.","Mortgage.","Shared.","Social_LA.","Social_Other.",
                 "Rented_Landlord.","Rented_Other.","RentFree.","Occupancy_Rooms.",
                 "Occupancy_Bedrooms.",
                 "PartTime.","FullTime.","Self.Emp.",
                 "FTStudent.","Student_Inactive.","Carer.","Sick.",
                 "Inactive_Other.","Unemp_16to24.","Unemp_50to74.","Unemp_NeverWorked.","Unemp_LongTerm.",
                 "T2W_Metro.","T2W_Train.","T2W_Bus.",
                 "T2W_Taxi.","T2W_Mbike.","T2W_Car.","T2W_Passenger.","T2W_Cycle.",
                  "T2W_Foot.","T2W_Other.","T2W_NoEmp.","All_Grade.","SocGrade_AB.",
                 "SocGrade_C1.","SocGrade_C2.","SocGrade_DE.","pHeating_None","pHeating_Gas",
                 "pHeating_Electric","pHeating_Other","Dwellings","Crr_EE","Ptn_EE",
                 "pop2011","area_km","dense_2011",
                 "median_household_income", "mean_household.size","mean_rooms")]
mydata <- data.matrix(mydata)
correlations <- rcorr(x = mydata, type="pearson")
correlations <- correlations$r
for(i in 1:nrow(correlations)){
  correlations[i,i] <- 0
}
max_coor <- apply(correlations, 1, max)
max_coor = max_coor[order(max_coor, decreasing = TRUE)]
head(max_coor)

fit <- lm(MeanDomGas_11_kWh ~ ., data=mydata, na.action = na.exclude)
calc.relimp(fit,type=c("lmg"),
            rela=TRUE)


step <- stepAIC(fit, direction="both")
step$anova # display results 
#plot(fit)

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 