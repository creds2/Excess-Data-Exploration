library(data.table)
library(ggplot2)
source("Tim/RScripts/Utility Scripts/multiplot.r")

c13<-fread("Tim/TempTables/13KmeansClusters080519.csv")
c12<-fread("Tim/TempTables/12KmeansClusters080519.csv")
names(c12)[2]<-"lsoa";names(c13)[2]<-"lsoa"
c12[,cluster_house:=as.integer(cluster_house)];c13[,cluster_house:=as.integer(cluster_house)]


lsoac<-fread("data-prepared/LSOAC_2011.csv")

#Allocate letters to cluster according to total domestic energy
ord13<-c13[,mean(gasAv+elecAv,na.rm=TRUE),by=cluster_house]
names(ord13)[2]<-"dom"; ord13<-ord13[order(-ord13$dom)];ord13$cluster<-LETTERS[1:13]
c13<-merge(c13,ord13[,.(cluster_house,cluster)],by="cluster_house")

ord12<-c12[,mean(gasAv+elecAv,na.rm=TRUE),by=cluster_house]
names(ord12)[2]<-"dom"; ord12<-ord12[order(-ord12$dom)];ord12$cluster<-LETTERS[1:12]
c12<-merge(c12,ord12[,.(cluster_house,cluster)],by="cluster_house")

#Plot energy usage to see effect on order of clusters
ggplot(c13, aes(cluster, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity") + 
  ggsave("Tim/plots/house_cluster_electricity13ord.jpg")

ggplot(c13, aes(cluster, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas") + 
  ggsave("Tim/plots/house_cluster_gas13ord.jpg")

ggplot(c12, aes(cluster, elecAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Electricity") + 
  ggsave("Tim/plots/house_cluster_electricity12ord.jpg")

ggplot(c12, aes(cluster, gasAv)) +
  geom_boxplot() +
  labs(x = "Cluster",
       y = "Household Gas") + 
  ggsave("Tim/plots/house_cluster_gas12ord.jpg")

write.csv(ord12,"Tim/TempTables/12KmeansClusters(with letters)080519.csv")
write.csv(ord13,"Tim/TempTables/13KmeansClusters(with letters)080519.csv")








#Plot groups
groups<-merge(lsoac[,.(lsoa,groupName)],c12[,lsoa,cluster],by="lsoa")
pn<-1
for (g in unique(groups$groupName)){
  #g<-"Cosmopolitan student neighbourhoods" 
  data<-groups[groupName==g,][order(-cluster)]
  p<- ggplot(data,aes(cluster,groupName))+geom_bar(stat="identity")+
    labs(title=g,x="Cluster",y="")+ theme(axis.text.y = element_blank(), axis.ticks = element_blank())+
  theme(plot.title = element_text(color = "blue", size = 10, face = "bold"))
  
  assign(paste0("p",pn),p)  
  pn=pn+1  
}

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,cols=3)
multiplot(p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,cols=3)

#plot Supergroups
sgroups<-merge(lsoac[,.(lsoa,supergroupName)],c12[,lsoa,cluster],by="lsoa")
pn<-1
for (sg in unique(sgroups$supergroupName)){
  #g<-"Cosmopolitan student neighbourhoods" 
  data<-sgroups[supergroupName==sg,][order(-cluster)]
  p<- ggplot(data,aes(cluster,supergroupName))+geom_bar(stat="identity")+
    labs(title=sg,x="Cluster",y="")+ theme(axis.text.y = element_blank(), axis.ticks = element_blank())+
    theme(plot.title = element_text(color = "blue", size = 10, face = "bold"))
  
  assign(paste0("p",pn),p)  
  pn=pn+1  
}

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=2)





