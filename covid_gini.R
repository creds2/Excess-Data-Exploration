# covid
library(sf)
library(dplyr)
library(lctools)
library(lubridate)
library(tidyr)

download.file("https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv", "data/MSOAs_latest.csv",
              mode = "wb")

covid <- read.csv("data/MSOAs_latest.csv")
covid$date <- ymd(covid$date)
covid$week <- as.numeric(covid$date - min(covid$date)) / 7 + 11
covid$week <- paste0("wk_",covid$week)

covid <- covid[,c("areaCode","week","newCasesBySpecimenDateRollingSum")]
covid$newCasesBySpecimenDateRollingSum[is.na(covid$newCasesBySpecimenDateRollingSum)] <- 0
covid_wide <- pivot_wider(covid, names_from = "week", values_from = "newCasesBySpecimenDateRollingSum")


msoa <- read_sf("C:/Users/malco/Downloads/infuse_msoa_lyr_2011_clipped/infuse_msoa_lyr_2011_clipped.shp")
msoa <- st_centroid(msoa)
msoa <- msoa[msoa$geo_code %in% covid_wide$areaCode, ]
msoa <- msoa[,c("geo_code","geometry")]

msoa <- left_join(msoa, covid_wide, by = c("geo_code" = "areaCode"))

msoa2 <- cbind(st_drop_geometry(msoa), st_coordinates(msoa))
#msoa <- as(msoa, "Spatial")

gini <- spGini(msoa2[,c("X","Y")],12,msoa2[,"wk_39"])


imp <- list()

for(i in 2:34){
  message(i)
  imp[[i]] <- msoa2[,c(i,35,36)]
  #res[[i]] <- spGini(msoa2[,c("X","Y")],12,msoa2[,i])
}

get_gini <- function(sub){
  lctools::spGini(sub[,c("X","Y")],12,sub[,1])
}



cl <- parallel::makeCluster(4)
res <- pbapply::pblapply(imp[2:length(imp)], get_gini, cl = cl)
parallel::stopCluster(cl)

saveRDS(res,"res_2020-10-31.Rds")

gini <- lapply(res, `[[`, 1)
gini <- unlist(gini)
plot(gini)

ginidf <- data.frame(week = paste0("Week ",11:43), gini = gini)

library(ggplot2)

ggplot(ginidf, aes(x = week, y = gini)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0.4,1) +
  ylab("Spatail Gini Coefficent") +
  xlab("PHE Week Number")
