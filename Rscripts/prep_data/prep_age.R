# Prep property age

library(dplyr)

age <- read.csv("data-input/VOA/voapropertyage.csv")
names(age)[1] <- c("lsoa")

saveRDS(age,"data-prepared/age.Rds")
