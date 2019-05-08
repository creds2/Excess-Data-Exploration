library(data.table)
library(ggplot2)
library(cowplot)
data<-fread("x:/data/all_cluster_variables.csv")
energy<-fread("x:/data/EnergyAndData (from New Devs).csv")

d<-merge(energy[,c("LSOA11CD","Region","elecAv","gasAv","nrgHH" )],data[,],by.x="LSOA11CD",by.y="LSOA")

d[,mean(elecAv), by = "cluster"]
d[,mean(gasAv,na.rm=TRUE), by = "cluster"]
d[,mean(nrgHH), by = "cluster"]

d[,all:=elecAv+gasAv+nrgHH]


d[,dom:=gasAv+elecAv]
#d[,colour:=number(cluster)]
d[cluster=="F", colour:="red"]
d[cluster=="E", colour:="orange"]
d[cluster=="D", colour:="yellow"]
d[cluster=="C", colour:="green"]
d[cluster=="B", colour:="blue"]
d[cluster=="A", colour:="purple"]

#Look at clusters by region and energy use

#Domestic vs Vehicle
lb1 <- paste("R^2 == ", round(summary(lm(nrgHH~dom, d))$r.squared,3))
ggplot(d,aes(x=dom, y=nrgHH))+ geom_point(aes(color=cluster))+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))+
  labs(x="Average HH Domestic Energy (kWh)", y="Average HH Vehicle Energy (kWh)",
       #size="horsepower",  col="# of cylinders", shape="# of gears")
    col="Cluster")+ 
  geom_smooth(method='lm')+ 
    geom_text(x=10000, y=20000, label=lb1 ,parse=TRUE)
            
#  geom_text(label = format(cor(d$dom,d$nrgHH,use="complete.obs")^2, digits = 3), parse = TRUE))
  
#  geom_text(aes(x = 25, y = 300, label = format(summary(m)$r.squared, digits = 3), parse = TRUE))
#cor(d$dom,d$nrgHH,use="complete.obs")^2

#Look at Energy vs Income
lb1 <- paste("R^2 == ", round(summary(lm(all~Income, d))$r.squared,3))
ggplot(d,aes(x=Income, y=all))+ geom_point(aes(color=cluster))+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))+
  labs(y="Average HH Total Energy (kWh)", x="Average HH Income (£'000s)", col="Cluster")+ 
  geom_smooth(method='lm')+ 
  geom_text(x=10, y=60000, label=lb1 ,parse=TRUE)

lb1 <- paste("R^2 == ", round(summary(lm(dom~Income, d))$r.squared,3))
ggplot(d,aes(x=Income, y=dom))+ geom_point(aes(color=cluster))+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))+
  labs(y="Average HH Domestic Energy (kWh)", x="Average HH Income (£'000s)", col="Cluster")+ 
  geom_smooth(method='lm')+ 
  geom_text(x=10, y=50000, label=lb1 ,parse=TRUE)


lb1 <- paste("R^2 == ", round(summary(lm(nrgHH~Income, d))$r.squared,3))
ggplot(d,aes(x=Income, y=nrgHH))+ geom_point(aes(color=cluster))+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))+
  labs(y="Average HH Vehicle Energy (kWh)", x="Average HH Income (£'000s)", col="Cluster")+ 
  geom_smooth(method='lm')+ 
  geom_text(x=10, y=17000, label=lb1 ,parse=TRUE)




p1<-ggplot(d, aes(x=cluster, y=Income)) +
  geom_boxplot(aes(color=cluster))+labs(y="Income")+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))
p2<-ggplot(d, aes(x=cluster, y=all)) +
  geom_boxplot(aes(color=cluster))+labs(y="Total Energy")+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))
p3<-ggplot(d, aes(x=cluster, y=nrgHH)) +
  geom_boxplot(aes(color=cluster))+labs(y="Vehicle Energy")+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))
p4<-ggplot(d, aes(x=cluster, y=dom)) +
  geom_boxplot(aes(color=cluster))+labs(y="Domestic Energy")+
  scale_color_manual(values = c("purple", "blue", "green","yellow","orange","red"))

prow<-plot_grid(p1+ theme(legend.position="none"),p2+ theme(legend.position="none"),
          p3+ theme(legend.position="none"),p4+ theme(legend.position="none"))
legend <- get_legend(p1)
plot_grid( prow, legend, rel_widths = c(3, .3))


#Look at by Regions
d[Region=="East",Reg:="E"];d[Region=="London",Reg:="L"];d[Region=="NEast",Reg:="NE"];
d[Region=="SEast",Reg:="SE"];d[Region=="SWest",Reg:="SW"];d[Region=="WMid",Reg:="WM"];
d[Region=="YandH",Reg:="YH"];d[Region=="NWest",Reg:="NW"];


col8<-c("black","purple", "blue", "green","yellow","orange","red","coral3")

ggplot(d,aes(x=dom, y=nrgHH))+ geom_point(aes(color=Region))+
  scale_color_manual(values = c("black","purple", "blue", "green","yellow","orange","red","coral3"))+
  labs(x="Average HH Domestic Energy (kWh)", y="Average HH Vehicle Energy (kWh)",
       #size="horsepower",  col="# of cylinders", shape="# of gears")
       col="Cluster")
p1<-ggplot(d, aes(x=Reg, y=Income)) +
  geom_boxplot(aes(color=Reg))+labs(x="",y="Income")+
  scale_color_manual(values = col8)
p2<-ggplot(d, aes(x=Reg, y=all)) +
  geom_boxplot(aes(color=Reg))+labs(x="",y="Total Energy")+
  scale_color_manual(values = col8)
p3<-ggplot(d, aes(x=Reg, y=nrgHH)) +
  geom_boxplot(aes(color=Reg))+labs(x="",y="Vehicle Energy")+
  scale_color_manual(values = col8)
p4<-ggplot(d, aes(x=Reg, y=dom)) +
  geom_boxplot(aes(color=Reg))+labs(x="",y="Domestic Energy")+
  scale_color_manual(values = col8)

prow<-plot_grid(p1+ theme(legend.position="none"),p2+ theme(legend.position="none"),
                p3+ theme(legend.position="none"),p4+ theme(legend.position="none"))
legend <- get_legend(p1)
plot_grid( prow, legend, rel_widths = c(3, .3))
