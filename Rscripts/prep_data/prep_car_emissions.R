# process fuel efficnecy data
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
tmap_mode("plot")
path = "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.xlsx"
path = "E:/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.xlsx"


emissions = readxl::read_xlsx(path, sheet = "LSOA")
emissions = emissions[emissions$LSOA != "zBetween Keepers",]
emissions = emissions[emissions$LSOA != "zUnknown",]
emissions[3:18] = lapply(emissions[3:18], function(x){as.numeric(gsub("c","",x))})

# esna = na.omit(emissions)
# esna$co2total = esna$`Average CO2 (g/km)` * esna$Total
# lm1 <- lm(`co2total` ~ `0 g/km` + `1 - 50 g/km` + `51-75 g/km` + `76 - 90 g/km` +       
#           `91 - 100 g/km` + `101 - 110 g/km` + `111 - 130 g/km` + `131 - 150 g/km` + `151 - 170 g/km` +`171 - 190 g/km` +
#           `191 - 225 g/km` + `226 - 255 g/km`  + `Over 255 g/km` +  `Not known`, data = esna)
# summary(lm1)
#av_co2 = lm1$coefficients[2:15]
av_co2 <- c(5, 25.5, 63, 83,95.5,105.5, 120.5,  140.5, 160.5 , 180.5, 208,  240.5, 309, 166)
# De-obfiscate the data
es = emissions[1,]
desobs = function(es, 
                  av_co2 = c(5, 25.5, 63, 83,95.5,105.5, 120.5,  140.5, 160.5 , 180.5, 208,  240.5, 309, 166)){
  es_t <- t(es)
  es_vals <- es_t[3:16,] 
  es_vals <- as.numeric(es_vals)
  
  names(es_vals) = names(es)[3:16]
  n_known <- sum(es_vals, na.rm = TRUE)
  n_total <- as.numeric(es_t[17,])
  n_missing <- n_total - n_known
  bands_missing <- sum(is.na(es_vals))
  
  if(is.na(n_total)){
    es$diff <- NA
    return(es)
   
  } else if(n_missing == 0){
    es$diff <- 0
    return(es)
    
  }else{
    
    
    # Make ther perms matrix
    perms <- make_perms(min_value = 1, max_val = 4, cols = bands_missing, tots = n_missing)
    names(perms) <- names(es_vals)[is.na(es_vals)]
    
    knows <- es_vals[!is.na(es_vals)]
    knows <- matrix(knows, nrow = 1, dimnames = list(NULL, names(knows)))
    
    if(nrow(perms) > 1){
      knows <- knows[rep(1, nrow(perms)),]
      
    }
    knows <- as.data.frame(knows)
    
    
    perms <- cbind(perms, knows)
    perms <- perms[,names(es_vals)]
    
    val_tot = 0
    for(i in seq(1, 14)){
      val = perms[,i] * av_co2[i]
      val_tot = val_tot + val
    }
    
    perms$co2 <- val_tot / n_total
    perms$diff <- sqrt((perms$co2 - es$`Average CO2 (g/km)`)**2)
    perms <- perms[perms$diff == min(perms$diff),]
    
    perms <- perms[1,] # if more than one possability
    
    es_final <- cbind(perms, es[,c("LSOA","Fuel","Total","Average CO2 (g/km)","Average age (years)")])
    es_final <- es_final[, c(names(es),"diff")]
    # thry the perms
    return(es_final)
    
    
    
  }
  
  
  
  
}

make_perms <- function(min_value = 0, max_val = 2, cols = 4, tots = 2){
  
  seq_lth <- length(seq(min_value, max_val))
  tot_perms <-  seq_lth ^ cols
  cols_list <- list()
  # perms <- rep(0:max_val, times = seq_lth)
  for(i in seq(1, cols)){
    val1 = seq_lth ^ (i - 1)
    val2 = seq_lth ^ (cols - i)
    nxt_col <- rep(seq(min_value, max_val), each = val1)
    nxt_col <- rep(nxt_col, times = val2)
    if(length(nxt_col) != tot_perms){
      stop()
    }
    cols_list[[i]] <- nxt_col
  }
  mat <- dplyr::bind_cols(cols_list)
  mat <- as.matrix(mat)
  mat <- mat[rowSums(mat) == tots,, drop = FALSE]
  mat = as.data.frame(mat)
  
}

nrow(emissions)
emissions_list <- split(emissions, 1:nrow(emissions))
emissions_new <- pbapply::pblapply(emissions_list, desobs)
emissions <- dplyr::bind_rows(emissions_new)
nrow(emissions)



emissions_summary1 = emissions[,c("LSOA","Fuel","Average CO2 (g/km)")]
emissions_summary2 = emissions[,c("LSOA","Fuel","Total")]
emissions_summary1 = emissions_summary1 %>%
  spread(Fuel, `Average CO2 (g/km)`)
emissions_summary2 = emissions_summary2 %>%
  spread(Fuel, Total)      
emissions_summary2$all_cars = rowSums(emissions_summary2[,2:6], na.rm = TRUE)


names(emissions_summary1) <- paste0(tolower(names(emissions_summary1)),"_co2")
names(emissions_summary2) <- paste0(tolower(names(emissions_summary2)),"_n")
names(emissions_summary1)[1] <- "LSOA"
names(emissions_summary2)[1] <- "LSOA"
emissions_summary = left_join(emissions_summary2, emissions_summary1, by = "LSOA")
# some lsoa have 200,000 cars! remove a few places
emissions_summary = emissions_summary[emissions_summary$all_cars_n < 5000, ]

#saveRDS(emissions_summary, "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.Rds")
saveRDS(emissions_summary, "E:/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_emissions.Rds")

# De-obfiscate the data
# emissions_summary_all = emissions_summary
# names(emissions_summary_all) <- gsub(" ","_",names(emissions_summary_all))
# emissions_summary_all <- split(emissions_summary_all, 1:nrow(emissions_summary_all))
# 
# es = emissions_summary_all[[1]]
# 
# desobs = function(es){
#   n_known = sum(es$diesel_n, es$electric_diesel_n, es$hybrid_electric_n, es$other_n, es$petrol_n, na.rm = TRUE)
#   
#   
# }



# Make some plots

bounds <- read_sf("data-prepared/LSOA_forplots.gpkg")
bounds <- left_join(bounds, emissions_summary, by = c("LSOA11" = "LSOA"))

map <- tm_shape(bounds) +
  tm_fill(col = "petrol_co2",
          breaks = c(100,120,130,140,150,160,170,180,200,250),
          palette = "-RdYlBu") +
  tm_layout(title = "Average CO2 emissions of petrol vehicles per km")

tmap_save(map, file = "plots/emissions_petrol_ave_co2.png")

map <- tm_shape(bounds) +
  tm_fill(col = "diesel_co2",
          breaks = c(100,120,130,140,150,160,170,180,200,250),
          palette = "-RdYlBu") +
  tm_layout(title = "Average CO2 emissions of diesel vehicles per km")

tmap_save(map, file = "plots/emissions_diesel_ave_co2.png")

summmary(bounds$diesel_co2)


