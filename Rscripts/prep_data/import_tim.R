# Import Tim's New data
folder = "E:/OneDrive - University of Leeds/CREDS Data/Tim Share/From Tim"

#Census Data LSOA
census_lsoa <- read.csv(paste0(folder,"/CensusData_LSOA(86).csv"), stringsAsFactors = F)
census_oa <- read.csv(paste0(folder,"/CensusData_OA(86).csv"), stringsAsFactors = F)

saveRDS(census_lsoa,"data-prepared/census_lsoa.Rds")
saveRDS(census_oa,"data-prepared/census_oa.Rds")

# MOT Data
mot_2011 <- read.csv(paste0(folder,"/MOT Data RACv9.3/MOT Data RACv9.3 LSOAoutputs_2011.csv"), 
                     stringsAsFactors = F)
names(mot_2011) <- c("LSOA", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                     "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                     "pmiles_car","pmiles_vans","cars_percap","miles_percap")

mot_2010 <- read.csv(paste0(folder,"/MOT Data RACv9.3/MOT Data RACv9.3 LSOAoutputs_2010.csv"), 
                     stringsAsFactors = F)
names(mot_2010) <- c("LSOA", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                     "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                     "pmiles_car","pmiles_vans","cars_percap","miles_percap")

mot_2009 <- read.csv(paste0(folder,"/MOT Data RACv9.3/MOT Data RACv9.3 LSOAoutputs_2009.csv"), 
                     stringsAsFactors = F)
names(mot_2009) <- c("LSOA", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                     "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                     "pmiles_car","pmiles_vans","cars_percap","miles_percap")

saveRDS(mot_2011,"../Secure-Data/Excess/mot_2011.Rds")
saveRDS(mot_2010,"../Secure-Data/Excess/mot_2010.Rds")
saveRDS(mot_2009,"../Secure-Data/Excess/mot_2009.Rds")
