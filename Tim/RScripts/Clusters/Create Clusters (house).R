# Cluster by basic building characteritics
library(dplyr)
library(ggplot2)
library(xgboost)
library(Hmisc)
library(data.table)

#lsoa_house<-fread("./TempTables/lsoa_house_Table_3(nonscaled).csv" )
lsoa_house<-read.csv("./TempTables/lsoa_house_Table_3(nonscaled).csv" )
# scale values 0 - 100
lsoa_clustering <- lsoa_house[,sapply(lsoa_house,class) %in% c("integer","numeric")]
for(i in 1:ncol(lsoa_clustering)){
  lsoa_clustering[[i]] <- lsoa_clustering[[i]] / max(lsoa_clustering[[i]]) * 100
}

clusters <- kmeans(lsoa_clustering, 12)
lsoa_house$cluster_house_nrg <- as.numeric(clusters$cluster)


lsoa_clustering<-subset(lsoa_clustering,select=-c(elecAv,gasAv))
#lsoa_clustering %>% select(-elecAv,-gasAv)%>%
clusters <- kmeans(lsoa_clustering, 12)
lsoa_house$cluster_house_NOnrg <- as.numeric(clusters$cluster)

c12<-as.data.table(lsoa_house)
#Allocate letters Caps = no NRG
ord12<-c12[,mean(gasAv+elecAv,na.rm=TRUE),by=cluster_house_NOnrg]
names(ord12)[2]<-"dom"; 
ord12<-ord12[order(-ord12$dom)];
ord12$cluster_NOnrg<-LETTERS[1:12]
c12<-merge(c12,ord12[,.(cluster_house_NOnrg,cluster_NOnrg)],by="cluster_house_NOnrg")

ord12<-c12[,mean(gasAv+elecAv,na.rm=TRUE),by=cluster_house_nrg]
names(ord12)[2]<-"dom"; 
ord12<-ord12[order(-ord12$dom)];
ord12$cluster_nrg<-letters[1:12]
c12<-merge(c12,ord12[,.(cluster_house_nrg,cluster_nrg)],by="cluster_house_nrg")

c12[,dom:=elecAv+gasAv]
fwrite(c12,"./TempTables/lsoa_house_12 clusters(nrg and NOnrg).csv")
