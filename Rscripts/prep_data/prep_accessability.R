# prep acessability

access_town <- readxl::read_excel("data-input/DFT Acessability/town centres.xls", sheet = "ACS0508-2013")
access_town <- as.data.frame(access_town)
names(access_town) <- access_town[6,]
access_town <- access_town[7:nrow(access_town),]
access_town <- access_town[!is.na(access_town$`LA Code`),]
access_town[6:ncol(access_town)] <- lapply(access_town[6:ncol(access_town)], as.numeric) 


saveRDS(access_town, "data-prepared/acess_town.Rds")
