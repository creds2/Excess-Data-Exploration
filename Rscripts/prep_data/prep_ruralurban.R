# Prep Rural Urban

ru <- read.csv("data-input/Rural Urban/Rural_Urban_Classification_2011_of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv")
ru <- ru[,c(1,4)]
names(ru) <- c("LSOA11","RUC11")
ru$RUC11 <- as.factor(ru$RUC11)

saveRDS(ru, "data-prepared/ruralurban.Rds")
