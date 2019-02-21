# Aim: This is Malcolm's rebuild of Tim's exploratory cluster analysis

# Packages and Setup ------------------------------------------------------
library(readr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(xgboost)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

# Old Analysis ----------------------------------------------------------------

# Reproduce some of Tim's work a proof of concept
# Domestic vs Vehicle
lb1 <- paste("R^2 == ", round(summary(lm(nrgHH~dom, lsoa))$r.squared,3))
ggplot(lsoa,aes(x=dom, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))+
  labs(x = "Average HH Domestic Energy (kWh)", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" )+ 
  geom_smooth(method = 'lm' ) + 
  geom_text(x = 10000, y = 20000, label = lb1, parse = FALSE) +
  ggsave("plots/lsoa_domestic_vs_vehicle.jpg")



# New Analysis ------------------------------------------------------------

# Check for basic correlations
lsoa_matrix <- data.matrix(lsoa) # Convert all data to numeric
correlations <- rcorr(x = lsoa_matrix, type="pearson")

correlations_domestic <- data.frame(R = correlations$r[,"dom"], P = correlations$P[,"dom"])
correlations_domestic$R <- round(correlations_domestic$R, 3)
correlations_domestic$P <- round(correlations_domestic$P, 3)
correlations_domestic <- correlations_domestic[order(correlations_domestic$R),]
correlations_domestic[correlations_domestic$R > 0.5 | correlations_domestic$R < -0.5,]
write.csv(correlations_domestic,"data/correlations_domestic_energy.csv")

correlations_vehicle <- data.frame(R = correlations$r[,"nrgHH"], P = correlations$P[,"nrgHH"])
correlations_vehicle$R <- round(correlations_vehicle$R, 3)
correlations_vehicle$P <- round(correlations_vehicle$P, 3)
correlations_vehicle <- correlations_vehicle[order(correlations_vehicle$R),]
correlations_vehicle[correlations_vehicle$R > 0.5 | correlations_vehicle$R < -0.5,]
write.csv(correlations_vehicle,"data/correlations_vehicle_energy.csv")

correlations_covariants <- melt(correlations$r)
correlations_covariants <- correlations_covariants[correlations_covariants$value > 0.5 | correlations_covariants$value < -0.5,]
names(correlations_covariants) <- c("from","to","value")
correlations_covariants$from <- as.character(correlations_covariants$from)
correlations_covariants$to <- as.character(correlations_covariants$to)
# Remove AB duplicates
# for(i in 1:nrow(correlations_covariants)){
#   vals <- c(correlations_covariants$from[i],correlations_covariants$to[i])
#   vals <- vals[order(vals)]
#   correlations_covariants$from[i] <- vals[1]
#   correlations_covariants$to[i] <- vals[2]
# }
# correlations_covariants <- correlations_covariants[!duplicated(correlations_covariants),]
correlations_covariants <- correlations_covariants[correlations_covariants$from != correlations_covariants$to,]
write.csv(correlations_covariants,"data/top_covariants.csv")

# Lets plot some of the top relationships aginst Tim's clusters
tim_colours <- c("purple", "blue", "green","yellow","orange","red")


# Household Energy plots --------------------------------------------------

ggplot(lsoa,aes(x=DE, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%Social Class DE", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_class_DE.jpg")


ggplot(lsoa,aes(x=Social, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Social Housing", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_social_housing.jpg")


ggplot(lsoa,aes(x=IMD, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "IMD Rank", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_IMD.jpg")

ggplot(lsoa,aes(x=Unemployed, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Unemployed", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_Unemployed.jpg")


ggplot(lsoa,aes(x=Rooms, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Mean Number of Rooms", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_Rooms.jpg")

ggplot(lsoa,aes(x=AB, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Class AB", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_AB.jpg")

ggplot(lsoa,aes(x=home, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%Work from home", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_Home.jpg")

ggplot(lsoa,aes(x=Income, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Median HH Income", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_Income.jpg")

ggplot(lsoa,aes(x=Outright, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Homes owned Outright", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_Outright.jpg")

ggplot(lsoa,aes(x=SelfEmployed, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Self-Employed", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_SelfEmpolyed.jpg")

ggplot(lsoa,aes(x=Detached, y=dom)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Detatched Houses", 
       y = "Average HH Domestic Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_Detached.jpg")


# Vehicle Energy Plots ----------------------------------------------------

ggplot(lsoa,aes(x=percNoCar, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% HH No Car", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_No_Car.jpg")

ggplot(lsoa,aes(x=IMD, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Index of Multiple Deprivation", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_IMD.jpg")

ggplot(lsoa,aes(x=Single, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%HH Single Parents", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Single.jpg")

ggplot(lsoa,aes(x=Unemployed, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Unemployed ", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Unemployed.jpg")

ggplot(lsoa,aes(x=Single, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%HH Single Parents", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Single.jpg")

ggplot(lsoa,aes(x=Flat, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%Flats", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Flat.jpg")

ggplot(lsoa,aes(x=Sick, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%HH Sick", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Sick.jpg")

ggplot(lsoa,aes(x=DE, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% DE", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_DE.jpg")

ggplot(lsoa,aes(x=Social, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% Social Housing", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Social.jpg")



ggplot(lsoa,aes(x=carsHH, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Mean Number of cars across all HH", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_carsHH.jpg")

ggplot(lsoa,aes(x=motor, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "%Travel to Work by car/morobike", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_motor.jpg")

ggplot(lsoa,aes(x=NoKids, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "% HH No Children", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_NoKids.jpg")

ggplot(lsoa,aes(x=AvT2W, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Average Travel to Work distance", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_AvT2W.jpg")

ggplot(lsoa,aes(x=RUC11CD, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Rural/Urban Classification (8)", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_RUC11CD.jpg")

ggplot(lsoa,aes(x=Rooms, y=nrgHH)) +
  geom_point(aes(color=cluster), size = 0.5) +
  scale_color_manual(values = tim_colours)+
  labs(x = "Mean Number of Rooms", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_vehicle_vs_Rooms.jpg")

# Clusters ----------------------------------------------------------------



# Cluster based on area and income
lsoa$RuralUrban <- as.numeric(as.factor(lsoa$RUC11CD)) # Convert RuralUrban or an numeric number
lsoa_clustering <- lsoa[,c("Income","RuralUrban","Rooms","carsHH","motor","AvT2W")]

wss <- sapply(1:15,function(k){kmeans(lsoa_clustering, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clusters <- kmeans(lsoa_clustering, 5)
lsoa$cluster2 <- as.factor(clusters$cluster)


# Domestic vs Vehicle - New Clusters
ggplot(lsoa,aes(x=dom, y=nrgHH)) +
  geom_point(aes(color=cluster2), size = 0.5, alpha = 0.5) +
  scale_colour_brewer(type = "qual", palette = "Set1") +
  stat_ellipse(aes(x=dom, y=nrgHH,color=cluster2),type = "norm", size = 1) +
  labs(x = "Average HH Domestic Energy (kWh)", 
       y = "Average HH Vehicle Energy (kWh)",
       col = "Cluster" ) + 
  ggsave("plots/lsoa_domestic_vs_vehicle_new_cluster.jpg")


ggplot(lsoa,aes(x=Income, y=motor)) +
  geom_point(aes(color=cluster2), size = 0.5, alpha = 0.5) +
  scale_colour_brewer(type = "qual", palette = "Set1") +
  stat_ellipse(aes(x=Income, y=motor,color=cluster2),type = "norm", size = 1) +
  labs(x = "Median HH Income",
       y = "%Travel to Work by car/morobike", 
       col = "Cluster" ) + 
 # ylim(0,100) +
  ggsave("plots/lsoa_income_vs_T2W.jpg")


# Box plots for new cluster -----------------------------------------------


ggplot(lsoa, aes(cluster2, Income)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Median HH Income") + 
  ggsave("plots/new_cluster_income.jpg")


ggplot(lsoa, aes(cluster2, Rooms)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Mean Number of Rooms") + 
  ggsave("plots/new_cluster_Rooms.jpg")


ggplot(lsoa, aes(cluster2, carsHH)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Mean Number of cars across all HH") + 
  ggsave("plots/new_cluster_carsHH.jpg")



ggplot(lsoa, aes(cluster2, motor)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "%Travel to Work by car/morobike") + 
  ggsave("plots/new_cluster_motor.jpg")

ggplot(lsoa, aes(cluster2, AvT2W)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Average Travel to Work distance") + 
  ggsave("plots/new_cluster_AvT2W.jpg")

ggplot(lsoa, aes(cluster2, dom)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Average HH Domestic Energy (kWh)") + 
  ggsave("plots/new_cluster_dom.jpg")


# Try some models ---------------------------------------------------------
lsoa_model <- lsoa[,!names(lsoa) %in% c("LSOA11CD","dom","SGN","SupergroupCode","SupergroupName","Region","Reg","cluster","cluster2","RU","RUC11CD")]
lsoa_model$gasAv[is.na(lsoa_model$gasAv)] <- 0
lsoa_model$all <- lsoa_model$gasAv + lsoa_model$elecAv + lsoa_model$nrgHH

lsoa_sample <- lsoa_model[sample(1:nrow(lsoa_model), round(nrow(lsoa_model)/10)),]
lsoa_sample_mat <- as.matrix(lsoa_sample[,!colnames(lsoa_sample) %in% c("nrgHH","gasAv","elecAv","all")])
lsoa_model_mat <- as.matrix(lsoa_model[,!colnames(lsoa_model) %in% c("nrgHH","gasAv","elecAv","all")])


# Electricity
m_elec = xgboost(data = lsoa_sample_mat,
                 label = lsoa_sample$elecAv, nrounds = 10, max_depth = 5)

plot(lsoa_model$elecAv, predict(m_elec, lsoa_model_mat)) +
abline(0,1,col = "red")


importance_m_elec = xgb.importance(model = m_elec, feature_names = colnames(lsoa_model_mat))
png(filename="plots/importance_electricity.png")
xgb.plot.importance(importance_m_elec, top_n = 15)
dev.off()

# Gas
m_gas = xgboost(data = lsoa_sample_mat,
                 label = lsoa_sample$gasAv, nrounds = 10, max_depth = 5)

plot(lsoa_model$gasAv, predict(m_gas, lsoa_model_mat)) +
  abline(0,1,col = "red")

importance_m_gas = xgb.importance(model = m_gas, feature_names = colnames(lsoa_model_mat))
png(filename="plots/importance_gas.png")
xgb.plot.importance(importance_m_gas, top_n = 15)
dev.off()

# Vehicles
m_vehicles = xgboost(data = lsoa_sample_mat,
                label = lsoa_sample$nrgHH, nrounds = 10, max_depth = 5)

plot(lsoa_model$nrgHH, predict(m_vehicles, lsoa_model_mat)) +
  abline(0,1,col = "red")

importance_m_vehicles = xgb.importance(model = m_vehicles, feature_names = colnames(lsoa_model_mat))
png(filename="plots/importance_vehicles.png")
xgb.plot.importance(importance_m_vehicles, top_n = 15)
dev.off()

# All
m_all = xgboost(data = lsoa_sample_mat,
                     label = lsoa_sample$all, nrounds = 10, max_depth = 5)

plot(lsoa_model$all, predict(m_all, lsoa_model_mat)) +
  abline(0,1,col = "red")

importance_m_all = xgb.importance(model = m_all, feature_names = colnames(lsoa_model_mat))
png(filename="plots/importance_all.png")
xgb.plot.importance(importance_m_all, top_n = 15)
dev.off()
