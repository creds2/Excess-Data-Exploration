# prep central heating
library(dplyr)

ch <- read.csv("data-input/Central Heating/c11qs415ukoa.csv")
names(ch) <- c("OA","All","No CH","Gas",
                "Electric","Oil","Solid fuel","Other",
                "Two or more","MODE_VAL","MODE_TYPE_MAJOR","MODE_PC",
                "SECOND_VAL","SECOND_TYPE_MAJOR","SECOND_PC",
               "SECOND_FIRST_RATIO")

ch <- ch[,c("OA","All","No CH","Gas",
            "Electric","Oil","Solid fuel","Other",
            "Two or more")]

lookup <- read.csv("data-input/OA Lookup/oa_lookup_dec_2011.csv", stringsAsFactors = F)
lookup <- lookup[,1:2]
names(lookup) <- c("OA11","LSOA11")

ch <- left_join(ch, lookup, by = c("OA" = "OA11"))
ch <- ch[!is.na(ch$LSOA11),]

ch_lsoa <- ch %>%
  select(-OA) %>%
  group_by(LSOA11) %>%
  summarise_all(sum)


saveRDS(ch_lsoa,"data-prepared/central_heating.Rds")
