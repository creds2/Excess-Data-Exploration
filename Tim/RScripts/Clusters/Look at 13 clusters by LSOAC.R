library(data.table)

source("Tim/RScripts/Utility Scripts/multiplot.r")

c13<-fread("Tim/TempTables/13KmeansClusters080519.csv")
c12<-fread("Tim/TempTables/12KmeansClusters080519.csv")
names(c12)[2]<-"lsoa";names(c13)[2]<-"lsoa"
c12[,cluster_house:=as.integer(cluster_house)];c13[,cluster_house:=as.integer(cluster_house)]

#Allocate letters to cluster according to total domestic energy
ord12<-c12[,mean(gasAv+elecAv,na.rm=TRUE),by=cluster_house]
names(ord12)[2]<-"dom"; ord12<-ord12[order(-ord12$dom)];ord12$cluster<-LETTERS[1:12]
c12<-merge(c12,ord12[,.(cluster_house,cluster)],by="cluster_house")

ord12<-c12[,mean(gasAv+elecAv,na.rm=TRUE),by=cluster_house]
names(ord12)[2]<-"dom"; ord12<-ord12[order(-ord12$dom)];ord12$cluster<-LETTERS[1:12]
c12<-merge(c12,ord12[,.(cluster_house,cluster)],by="cluster_house")

lsoac<-fread("data-prepared/LSOAC_2011.csv")

groups<-merge(lsoac[,.(lsoa,groupName)],c12[,lsoa,cluster_house],by="lsoa")

pn<-1
for (g in unique(g$groupName){
  #g<-"Cosmopolitan student neighbourhoods" 
  data<-groups[groupName==g,][order(-cluster_house)]
  p<-
    ggplot(data,aes(cluster_house,groupName))+geom_bar(stat="identity")
  +coord_flip()+
    labs(title=g,x="LSOAC")
  #+  scale_x_discrete(limits = rev(levels(SupergroupName)))
  
  assign(paste0("p",pn),p)  
  pn=pn+1  
}

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=2)




pn<-1
for (g in supgroups$supergroupName){
  #g<-"Cosmopolitan student neighbourhoods" 
  data<-supgroupmatch[supergroupName==g,][order(-N)]
  p<-ggplot(data,aes(SupergroupName,N))+geom_bar(stat="identity")+coord_flip()+
    labs(title=g,x="OAC")
  #+  scale_x_discrete(limits = rev(levels(SupergroupName)))
  
  assign(paste0("p",pn),p)  
  pn=pn+1  
}

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=2)




