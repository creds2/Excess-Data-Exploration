# prep household censu data

ks204EW <- read.csv("data-input/Census/ks204EW.csv", stringsAsFactors = FALSE)
names(ks204EW) <- c("date",
                    "geography",
                    "LSOA11",
                    "Rural.Urban",
                    "All",
                    "BornUK",
                    "Country.of.Birth..England..measures..Value",
                    "Country.of.Birth..Northern.Ireland..measures..Value",
                    "Country.of.Birth..Scotland..measures..Value",
                    "Country.of.Birth..Wales..measures..Value",
                    "Country.of.Birth..United.Kingdom.not.otherwise.specified..measures..Value",
                    "BornIreland",
                    "BornOtherEU",
                    "Country.of.Birth..Other.EU..Member.countries.in.March.2001..measures..Value",
                    "Country.of.Birth..Other.EU..Accession.countries.April.2001.to.March.2011..measures..Value",
                    "BornNonEU")

ks204EW <- ks204EW[,c("LSOA11","All","BornUK","BornIreland","BornOtherEU","BornNonEU")]
ks204EW$pBornUK <- ks204EW$BornUK / ks204EW$All
ks204EW$pBornEU <- (ks204EW$BornIreland +  ks204EW$BornOtherEU) / ks204EW$All
ks204EW$pBornNonEU <- ks204EW$BornNonEU / ks204EW$All
ks204EW <- ks204EW[,c("LSOA11","pBornUK","pBornEU","pBornNonEU")]

saveRDS(ks204EW,"data-prepared/country_birth.Rds")
