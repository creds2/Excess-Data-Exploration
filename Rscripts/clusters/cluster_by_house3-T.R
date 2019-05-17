# Cluster by basic building characteritics
library(dplyr)
library(ggplot2)
library(xgboost)
library(Hmisc)
library(data.table)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
#I couldn't work out how to load the secure data stuff
unlink("temp", recursive = T)

age <- readRDS("data-prepared/age.Rds")
epc <- readRDS("data-prepared/EPC.Rds")
ch <- readRDS("data-prepared/central_heating.Rds")
pd <- readRDS("data-prepared/populationDensity.Rds")

# convert to average age
age$age_mean <- sapply(1:nrow(age), function(i){
  sub <- age[i,3:14]
  sub <- as.numeric(sub)
  res <- weighted.mean(x = c(1850,1909,1924,1934.5,1949.5,1959.5,1968.5,
                             1977.5,1987.5,1996,2004.5,2012.5) ,w = sub)
  return(res)
})
age <- age[,c("AREA_CODE","age_mean")]

# join togther
lsoa <- left_join(lsoa,age, by = c("LSOA11CD" = "lsoa"))
lsoa <- left_join(lsoa,epc, by = c("LSOA11CD" = "LSOA11"))
lsoa <- left_join(lsoa,ch, by = c("LSOA11CD" = "LSOA11"))
lsoa <- left_join(lsoa,pd, by = c("LSOA11CD" = "LSOA11"))

rm(ch,age,epc,pd)

lsoa_house <- lsoa[,c("LSOA11CD",
                      "elecAv","gasAv",
                      "RU","Income","Detached","SemiDetached","Terraced","Flat","Rooms","HHsize",
                      "age_mean","Dwellings","Crr_EE",
                      "All","No CH","Gas","Electric","Oil","Solid fuel","Other","dense_2011"
)]

lsoa_house$gasAv[is.na(lsoa_house$gasAv)] <- 0

lsoa_house$pHeating_None <- round(lsoa_house$`No CH` / lsoa_house$All * 100,2)
lsoa_house$pHeating_Gas <- round(lsoa_house$Gas / lsoa_house$All * 100,2)
lsoa_house$pHeating_Electric <- round(lsoa_house$Electric / lsoa_house$All * 100,2)
lsoa_house$pHeating_Other <- round((lsoa_house$Oil + lsoa_house$`Solid fuel` + lsoa_house$Other)/ lsoa_house$All * 100,2)

lsoa_house <- lsoa_house[,c("LSOA11CD","elecAv","gasAv","Income",
                            "Detached","SemiDetached","Terraced","Flat","Rooms",
                            "HHsize","age_mean","Crr_EE","pHeating_None","pHeating_Gas",
                            "pHeating_Electric","pHeating_Other","dense_2011")]

write.csv(lsoa_house,"./TempTables/lsoa_house_Table_3(nonscaled).csv" )



# Cluster
#rem these to keep lsoa code in
lsoa_clustering <- lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")]
pairs(sample_frac(lsoa_clustering, 0.01))
correlations <- rcorr(x = data.matrix(lsoa_clustering), type="pearson")
foo = correlations$r

# scale values 0 - 100
for(i in 1:ncol(lsoa_clustering)){
  lsoa_clustering[[i]] <- lsoa_clustering[[i]] / max(lsoa_clustering[[i]]) * 100
}


# Compare Opimum number of clusters
# wss <- sapply(1:15,function(k){kmeans(lsoa_clustering, k, nstart=50,iter.max = 15 )$tot.withinss})
# plot(1:15, wss,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")

#Choose 12 clusters for now
clusters <- kmeans(lsoa_clustering, 12)
lsoa_house$cluster_house <- as.numeric(clusters$cluster)

#I am not certain what '%>%' is here?
cluster_summary <- lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")] %>%
  group_by(cluster_house) %>%
  summarise_all(mean)


lsoa_house$cluster_house <- as.character(clusters$cluster)

ggplot(lsoa_house, aes(cluster_house, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity") + 
  ggsave("plots/house_cluster_scaled_electricity.jpg")

ggplot(lsoa_house, aes(cluster_house, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas") + 
  ggsave("plots/house_cluster_scaled_gas.jpg")

#write.csv(cluster_house,"../TempTables/CLuster_House_Table_3.csv")

# Do again without gas and electric values

lsoa_clustering <- lsoa_house[,!names(lsoa_house) %in% c("LSOA11CD","elecAv","gasAv","RU","cluster_house")]
# scale values 0 - 100
for(i in 1:ncol(lsoa_clustering)){
  lsoa_clustering[[i]] <- lsoa_clustering[[i]] / max(lsoa_clustering[[i]]) * 100
}

clusters <- kmeans(lsoa_clustering, 10)
lsoa_house$cluster_house <- as.numeric(clusters$cluster)

#I am not certain what '%>%' is here?
cluster_summary <- lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")] %>%
  group_by(cluster_house) %>%
  summarise_all(mean) %>%
  arrange(gasAv)

cluster_summary$cluster_house_name <- LETTERS[1:nrow(cluster_summary)]

lsoa_house$cluster_house_name <- cluster_summary$cluster_house_name[match(lsoa_house$cluster_house, cluster_summary$cluster_house)]

ggplot(lsoa_house, aes(cluster_house_name, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity")

ggplot(lsoa_house, aes(cluster_house_name, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas")


ggplot(lsoa_house, aes(cluster_house_name, Income)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Income")


### Clusther by differetn groupings

# Building Age
wss <- sapply(1:15,function(k){kmeans(lsoa_house[,c("Age_pre1918","Age_1919_44","Age_1945_64","Age_1965_82","Age_1983_99","Age_post2000")], k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:15, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
clusters <- kmeans(lsoa_clustering, 6)
lsoa_house$cluster_age <- as.character(clusters$cluster)


ggplot(lsoa_house, aes(cluster_age, Age_1965_82)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "gasAv")
