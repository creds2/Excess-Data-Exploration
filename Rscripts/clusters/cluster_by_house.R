# Cluster by basic building characteritics
library(dplyr)
library(ggplot2)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

age <- readRDS("data-prepared/age.Rds")
epc <- readRDS("data-prepared/EPC.Rds")
ch <- readRDS("data-prepared/central_heating.Rds")

# join togther

lsoa <- left_join(lsoa,age, by = c("LSOA11CD" = "lsoa"))
lsoa <- left_join(lsoa,epc, by = c("LSOA11CD" = "LSOA11"))
lsoa <- left_join(lsoa,ch, by = c("LSOA11CD" = "LSOA11"))

rm(ch,age,epc)

lsoa_house <- lsoa[,c("LSOA11CD",
                      "elecAv","gasAv",
                      "RU","Income","Detached","SemiDetached","Terraced","Flat","Rooms","HHsize",
                      "BP_PRE_1900","BP_1900_1918","BP_1919_1929","BP_1930_1939","BP_1945_1954",
                      "BP_1955_1964","BP_1965_1972","BP_1973_1982","BP_1983_1992","BP_1993_1999",
                      "BP_2000_2009","BP_2010_2015","BP_UNKNOWN","ALL_PROPERTIES",
                      "Dwellings","Crr_A","Ptn_A","Crr_EE","Ptn_EE","Crr_mode",
                      "All","No CH","Gas","Electric","Oil","Solid fuel","Other"
                      )]

lsoa_house$gasAv[is.na(lsoa_house$gasAv)] <- 0

# Look at importnace
lsoa_house_mat <- as.matrix(lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")])
lsoa_house_mat <- lsoa_house_mat[,4:ncol(lsoa_house_mat)]


# Electricity
m_elec = xgboost::xgboost(data = lsoa_house_mat,
                 label = lsoa_house$elecAv, nrounds = 10, max_depth = 5)

importance_m_elec = xgboost::xgb.importance(model = m_elec, feature_names = colnames(lsoa_house_mat))
xgboost::xgb.plot.importance(importance_m_elec, top_n = 15)

# Gas
m_gas = xgboost::xgboost(data = lsoa_house_mat,
                          label = lsoa_house$gasAv, nrounds = 10, max_depth = 5)

importance_m_gas = xgboost::xgb.importance(model = m_gas, feature_names = colnames(lsoa_house_mat))
xgboost::xgb.plot.importance(importance_m_gas, top_n = 15)

# Convert to %
lsoa_house$pAge_PRE_1900 <- round(lsoa_house$BP_PRE_1900 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1900_1918 <- round(lsoa_house$BP_1900_1918 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1919_1929 <- round(lsoa_house$BP_1919_1929 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1930_1939 <- round(lsoa_house$BP_1930_1939 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1945_1954 <- round(lsoa_house$BP_1945_1954 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1955_1964 <- round(lsoa_house$BP_1955_1964 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1965_1972 <- round(lsoa_house$BP_1965_1972 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1973_1982 <- round(lsoa_house$BP_1973_1982 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1983_1992 <- round(lsoa_house$BP_1983_1992 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_1993_1999 <- round(lsoa_house$BP_1993_1999 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_2000_2009 <- round(lsoa_house$BP_2000_2009 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_2010_2015<- round(lsoa_house$BP_2010_2015 / lsoa_house$ALL_PROPERTIES * 100,2)
lsoa_house$pAge_UNKNOWN <- round(lsoa_house$BP_UNKNOWN / lsoa_house$ALL_PROPERTIES * 100,2)

lsoa_house$pHeating_None <- round(lsoa_house$`No CH` / lsoa_house$All * 100,2)
lsoa_house$pHeating_Gas <- round(lsoa_house$Gas / lsoa_house$All * 100,2)
lsoa_house$pHeating_Electric <- round(lsoa_house$Electric / lsoa_house$All * 100,2)
lsoa_house$pHeating_Other <- round((lsoa_house$Oil + lsoa_house$`Solid fuel` + lsoa_house$Other)/ lsoa_house$All * 100,2)

lsoa_house <- lsoa_house[,c("LSOA11CD","elecAv","gasAv","RU","Income",
                            "Detached","SemiDetached","Terraced","Flat","Rooms",
                            "HHsize","pAge_PRE_1900","pAge_1900_1918","pAge_1919_1929","pAge_1930_1939","pAge_1955_1964",
                            "pAge_1945_1954","pAge_1965_1972","pAge_1973_1982","pAge_1983_1992","pAge_1993_1999",
                            "pAge_2000_2009","pAge_2010_2015","pAge_UNKNOWN","pHeating_None","pHeating_Gas",
                            "pHeating_Electric", "pHeating_Other")]
# Cluster

lsoa_clustering <- lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")]
lsoa_clustering <- lsoa_clustering[,!names(lsoa_clustering) %in% c("elecAv","gasAv","Income")]

wss <- sapply(1:15,function(k){kmeans(lsoa_clustering, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares")

clusters <- kmeans(lsoa_clustering, 7)
lsoa_house$cluster_house <- as.numeric(clusters$cluster)

cluster_summary <- lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")] %>%
  group_by(cluster_house) %>%
  summarise_all(mean)

# cluster_summary$elecAv <- cluster_summary$elecAv / max(cluster_summary$elecAv) * 100
# cluster_summary$gasAv <- cluster_summary$gasAv / max(cluster_summary$gasAv) * 100
cluster_summary$elecAv <- NULL
cluster_summary$gasAv <- NULL
cluster_summary$cluster_house <- as.character(cluster_summary$cluster_house)
cluster_summary$pHeating_None <- NULL
cluster_summary$pAge_UNKNOWN <- NULL
cluster_summary$Rooms <- NULL
cluster_summary$HHsize <- NULL

cluster_summary_melt <- reshape2::melt(cluster_summary)
ggplot(cluster_summary_melt, aes(y = value, x = variable, 
                group = cluster_house, colour = cluster_house)) + coord_polar() + geom_point() + geom_path() + 
  labs(x = NULL)



ggplot(lsoa_house, aes(cluster_house, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity") + 
  ggsave("plots/house_cluster_electricity.jpg")

ggplot(lsoa_house, aes(cluster_house, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas") + 
  ggsave("plots/house_cluster_gas.jpg")
