# Top 10 areas.

dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

elec <- c(tail(sort(lsoa$elecAv),10), head(sort(lsoa$elecAv),10))
gas <- c(tail(sort(lsoa$gasAv),10), head(sort(lsoa$gasAv),10))
tran <- c(tail(sort(lsoa$nrgHH),10), head(sort(lsoa$nrgHH),10))

top10 <- lsoa[lsoa$elecAv %in% elec |
                lsoa$gasAv %in% gas | 
                lsoa$nrgHH %in% tran 
                ,]
top10 <- top10[,c("LSOA11CD","Region","elecAv","gasAv","nrgHH","SupergroupName","RUC11CD")]

