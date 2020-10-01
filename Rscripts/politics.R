# local politics for LSOA
# https://medium.com/@opendatamcr/so-you-think-you-know-your-country-revealing-the-hidden-patterns-in-local-democracy-dd41e1fe29e5

pol <- read.csv("data-input/Politics/LPR_LSOA_WD_LAD.csv")

lsoa <- read.csv("data/interview_areas.csv")

pol <- pol[pol$LSOA11CD %in% lsoa$LSOA11,]
pol <- pol[,c(1,2,8:11)]

write.csv(pol,"data/interview_areas_politics.csv", row.names = FALSE)
