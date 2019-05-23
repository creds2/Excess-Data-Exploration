library(data.table)
library(ggplot2)
library(xgboost)
library(plyr)


c12<- fread("./TempTables/lsoa_house_12 clusters(nrg and NOnrg).csv")
lsoac<-fread("data-prepared/LSOAC_2011.csv")

#Plot energy use with and without 
ggplot(c12, aes(cluster_nrg, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity") + 
  ggsave("Tim/plots/house_cluster_electricity12ord.jpg")

ggplot(c12, aes(cluster_nrg, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas") + 
  ggsave("Tim/plots/house_cluster_gas12ord.jpg")

ggplot(c12, aes(cluster_NOnrg, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity") + 
  ggsave("Tim/plots/house_cluster_electricity12ord.jpg")

ggplot(c12, aes(cluster_NOnrg, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas") + 
  ggsave("Tim/plots/house_cluster_gas12ord.jpg")

ggplot(c12, aes(cluster_NOnrg, dom)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Domestic Energy") + 
  ggsave("Tim/plots/house_cluster_dom12ord.jpg")
#plot outliers a different colour
plot_Data <- ddply(c12, .(cluster_NOnrg), mutate, Q1=quantile(dom, 1/4), Q3=quantile(dom, 3/4), IQR=Q3-Q1, upper.limit=Q3+1.5*IQR, lower.limit=Q1-1.5*IQR)

ggplot(plot_Data, aes(x=cluster_NOnrg, y=dom)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Domestic Energy")+
  geom_point(data=plot_Data[plot_Data$dom > plot_Data$upper.limit ,],colour="red")
  ggsave("Tim/plots/house_cluster_dom12ord-redoutliers.jpg")




##########################
names(c12)
c12[,cluster_house_nrg:=NULL];c12[,cluster_house_NOnrg:=NULL]
c<-as.data.frame(c12[,!c("LSOA11CD","cluster_NOnrg","cluster_nrg" )])
c<-as.data.frame(c12)

# Look at importnace
lsoa_house_mat <- as.matrix(as.data.frame(c[,sapply(c,class) %in% c("integer","numeric")]))
#lsoa_house_mat <- lsoa_house_mat[,4:ncol(lsoa_house_mat)]


# Electricity
m_elec = xgboost::xgboost(data = lsoa_house_mat,
                          label = c$elecAv, nrounds = 10, max_depth = 5)
importance_m_elec = xgboost::xgb.importance(model = m_elec, feature_names = colnames(lsoa_house_mat))
xgboost::xgb.plot.importance(importance_m_elec, top_n = 15)

# Gas
# Gas
m_gas = xgboost::xgboost(data = lsoa_house_mat,
                         label = lsoa_house$gasAv, nrounds = 10, max_depth = 5)

importance_m_gas = xgboost::xgb.importance(model = m_gas, feature_names = colnames(lsoa_house_mat))
xgboost::xgb.plot.importance(importance_m_gas, top_n = 15)




