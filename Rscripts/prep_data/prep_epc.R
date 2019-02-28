# prep epc data
library(dplyr)

epc <- read.csv("data-input/EPC/epcoa.csv" , stringsAsFactors = F)
lookup <- read.csv("data-input/OA Lookup/oa_lookup_dec_2011.csv", stringsAsFactors = F)
lookup <- lookup[,1:2]
names(lookup) <- c("OA11","LSOA11")


epc <- left_join(epc, lookup, by = c("OA" = "OA11"))
summary(is.na(epc$LSOA11))

# Remove the few missing OAs
epc <- epc[!is.na(epc$Num_Dwell),]


# Convert % to numbers
epc$Crr_A <- epc$Num_Dwell * epc$Crr_pcA
epc$Ptn_A <- epc$Num_Dwell * epc$Ptn_pcA

epc_lsoa <- epc %>%
  group_by(LSOA11) %>%
  summarise(Dwellings = sum(Num_Dwell, na.rm = T),
            Crr_A = sum(Crr_A, na.rm = T),
            Ptn_A = round(sum(Ptn_A, na.rm = T)),
            Crr_EE = weighted.mean(Crr_mednEE, Num_Dwell, na.rm = T),
            Ptn_EE = weighted.mean(Ptn_mednEE, Num_Dwell, na.rm = T),
            Crr_mode = names(which.max(table(rep(Crr_modeEER,Num_Dwell)))),
            Ptn_mode = names(which.max(table(rep(Ptn_modeEER,Num_Dwell))))
            )

saveRDS(epc_lsoa,"data-prepared/EPC.Rds")
