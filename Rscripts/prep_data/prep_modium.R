# Prep Modium
library(dplyr)

modium <- read.csv("data-input/MODUM/modumew2016.csv", stringsAsFactors = F)
lookup <- read.csv("data-input/OA Lookup/oa_lookup_dec_2011.csv", stringsAsFactors = F)
lookup <- lookup[,1:2]
names(lookup) <- c("OA11","LSOA11")



modium <- left_join(modium, lookup, by = c("OA_CODE" = "OA11"))
summary(is.na(modium$LSOA11))

modium_lsoa <- modium %>%
  group_by(LSOA11) %>%
  summarise(Cluster_mode = names(which.max(table(CLUSTER_LABEL))),
            Numb_Clusters = length(unique(CLUSTER_LABEL))
            )

saveRDS(modium_lsoa,"data-prepared/modium.Rds")
