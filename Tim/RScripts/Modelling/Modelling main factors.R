# Cluster by basic building characteritics
library(dplyr)
library(ggplot2)
library(xgboost)
library(data.table)
#-----------------------------------------------------


data<-fread("data-prepared/tim_data.csv")
#Electricity
mstrE<-lm(elecAv~Rooms,data);summary(mstrE)
mstrE<-lm(elecAv~Rooms+pHeating_Gas,data);summary(mstrE)
mstrE<-lm(elecAv~Rooms+pHeating_Gas+pHeating_Electric,data);summary(mstrE)
mstrE<-lm(elecAv~Rooms+pHeating_Gas+pHeating_Electric+Flat,data);summary(mstrE)
mstrE<-lm(elecAv~Rooms+pHeating_Gas+pHeating_Electric+Flat+BP_1930_1939,data);summary(mstrE)
plot(predict(mstrE),data$elecAv,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

#Gas
mstrG<-lm(gasAv~Rooms,data);summary(mstrE)
mstrG<-lm(gasAv~Rooms+Flat,data);summary(mstrE)
mstrG<-lm(gasAv~Rooms+Flat+BP_1930_1939,data);summary(mstrE)
mstrG<-lm(gasAv~Rooms+Flat+BP_1930_1939+BP_1900_1918,data);summary(mstrE)
mstrG<-lm(gasAv~Rooms+Flat+BP_1930_1939+BP_1900_1918+pHeating_Gas,data);summary(mstrE)
mstrG<-lm(gasAv~Rooms+Flat+BP_1930_1939+BP_1900_1918+pHeating_Gas+pHeating_Other,data);summary(mstrE)



#Cars (All HH)
mstrC1<-lm(nrgHH~PopDens,data);summary(mstrC1)
mstrC1<-lm(nrgHH~PopDens+carsHH,data);summary(mstrC1)
mstrC1<-lm(nrgHH~carsHH+NoCarsHH,data);summary(mstrC1)

plot(predict(mstrC1),data$nrgHH,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


#Cars (All HH with Cars)
mstrC2<-lm(nrgHH~PopDens,data);summary(mstrC2)
mstrC2<-lm(nrgHH~PopDens+NoCarsHH,data);summary(mstrC2)
mstrC2<-lm(nrgHH~PopDens+NoCarsHH+T2W_Cycle.,data);summary(mstrC2)
mstrC2<-lm(nrgHH~PopDens+NoCarsHH+T2W_Cycle.+AvT2W,data);summary(mstrC2)
mstrC2<-lm(nrgHH~PopDens+NoCarsHH+T2W_Cycle.+AvT2W+carsHHcar,data);summary(mstrC2)

plot(predict(mstrC2),data$nrgHH,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


