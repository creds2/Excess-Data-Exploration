install.packages("C50")
library(rpart)
all_noNA <- all[!is.na(all$MeanDomGas_11_kWh), ]
all_noNA <- all_noNA[,!sapply(all_noNA, anyNA)]
all_noNA <- all_noNA[,!names(all_noNA) %in% c("DomMet_17","MeanDomElec_17_kWh","TotDomElec_17_kWh","GasMet_11",
                                             "TotDomGas_11_kWh","cars_total","cars_miles","pu5k",
                                             "TotDomGas_11_kWh","GasMet_11","MeanDomElec_17_kWh")]


model <- rpart(MeanDomGas_11_kWh ~ ., data=all_noNA, na.action = na.exclude)
summary(model)

names(all)


library(tree)

model_tree = tree(MeanDomGas_11_kWh ~ ., data=all_noNA, na.action = na.exclude)
summary(model_tree)
plot(model_tree)
text(model_tree, pretty = 0)

plot(all_noNA$MeanDomGas_11_kWh, predict(model_tree))

lm1 <- lm(MeanDomGas_11_kWh ~ mean_rooms, data=all_noNA, na.action = na.exclude)
summary(lm1)

lm2 <- lm(MeanDomGas_11_kWh ~ mean_rooms + Outright. + SocGrade_AB. +  northing + mean_household.size + mean_bedrooms, data=all_noNA, na.action = na.exclude)
summary(lm2)
