
# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

census <- readRDS("data-prepared/census_lsoa.Rds")
mot<-fread("../Secure-Data/Excess/MOT Data/2011_LSOA_Energy_and_emissionsv9a.csv")

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
#age <- age[,c("lsoa","age_mean")]

# join togther
lsoa <- left_join(lsoa,age, by = c("LSOA11CD" = "lsoa"))
lsoa <- left_join(lsoa,epc, by = c("LSOA11CD" = "LSOA11"))
lsoa <- left_join(lsoa,ch, by = c("LSOA11CD" = "LSOA11"))
lsoa <- left_join(lsoa,pd, by = c("LSOA11CD" = "LSOA11"))

rm(ch,age,epc,pd)

# join togther
l<-lsoa
lsoa$Age_Mean<-NULL;lsoa$NoCH<-NULL;
lsoa <- left_join(lsoa,census, by = c("LSOA11CD" = "CODE"))
lsoa<- left_join(lsoa,mot, by = c("LSOA11CD" = "lsoa"))
names(lsoa)[1]<-"lsoa"

lsoa$gasAv[is.na(lsoa$gasAv)] <- 0

lsoa$pHeating_None <- round(lsoa$`No CH` / lsoa$All * 100,2)
lsoa$pHeating_Gas <- round(lsoa$Gas / lsoa$All * 100,2)
lsoa$pHeating_Electric <- round(lsoa$Electric / lsoa$All * 100,2)
lsoa$pHeating_Other <- round((lsoa$Oil + lsoa$`Solid fuel` + lsoa$Other)/ lsoa$All * 100,2)

lsoa<-as.data.table(lsoa)
#Do MOT
mot<-fread("../Secure-Data/Excess/MOT Data/2011_LSOA_Energy_and_emissionsv9a.csv")
m<-mot[,.(lsoa,N,kmAv,sizeAv,nrgAv,nrgTot)]
m<-merge(m,lsoa[,c("lsoa","totalhh","hhnocar","hhwithcar")],by.x="lsoa",by.y="lsoa",all.y=TRUE)
m[,nrgHH:=nrgTot/totalhh][,nrgHHcar:=nrgTot/hhwithcar]
m[,pHHnocar:=hhnocar*100/totalhh];m[,pHHcar:=hhwithcar*100/totalhh]

lsoa[,c("totalhh","hhnocar","hhwithcar","nrgHH","nrgTot","kmAv","sizeAv","nrgAv","N"):=NULL]
lsoa<-merge(lsoa,m,by="lsoa",all.x=TRUE)
lsoa[,T2W_Active:=T2W_Cycle.+ T2W_Foot.]

fwrite(lsoa,"data-prepared/tim_data.csv")
