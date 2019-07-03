# Starts from the Modelling Summary.Rmd
# Have access to all and bounds in the envrionment

# remove secure data

all$median_household_income <- NULL
all <- all[,names(all)[!names(all) %in% c("cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                                         "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                                         "pmiles_car","pmiles_vans","cars_percap","miles_percap")]]


# join on the geometry
bounds <- st_transform(bounds, 4326)
all <- left_join(all,bounds, by = c("LSOA11"))


#put id column first
all$id <- 1:nrow(all)
col.names <- names(all)[names(all) != "id"]
all <- all[,c("id",col.names)]

#Reduce precison of data to reduce file size
all$geom <- st_as_binary(all$geom, precision = 100000)
all$geom <- st_as_sfc(all$geom)

#convert to well known text
all$geom <- st_as_text(all$geom)
all <- as.data.frame(all)

# Remove special characters from column names
names(all) <- gsub("[[:punct:]]", "", x = names(all))

# make 0 - 1 scaled to 0 - 100%
all$SocGradeAB <- all$SocGradeAB * 100
all$SocGradeC1 <- all$SocGradeC1 * 100
all$SocGradeC2 <- all$SocGradeC2 * 100
all$SocGradeDE <- all$SocGradeDE * 100 


#round off numbers
for(i in 1:ncol(all)){
  
  if(class(all[,i]) == "numeric"){
    all[,i] <- round(all[,i],2)
  }
}


#Write as csv
write.csv(all,"wisemover/creds_lsoa_public.csv", row.names = F, na = "")

  