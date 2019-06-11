# prep household censu data

ks403EW <- read.csv("data-input/Census/ks403EW.csv", stringsAsFactors = FALSE)
names(ks403EW) <- c("date",
                    "geography",
                    "geography.code",
                    "Rural.Urban",
                    "All.categories",
                    "noCH",
                    "hasCH",
                    "Occupancy_rooms_less1",
                    "Occupancy_bedrooms_less1",
                    "mean_household.size",
                    "mean_rooms",
                    "mean_bedrooms")

ks403EW <- ks403EW[,c("geography.code","mean_household.size","mean_rooms","mean_bedrooms")]
saveRDS(ks403EW,"data-prepared/rooms.Rds")
