library(ggplot2)
library(sf)
library(tmap)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
}

# Read In Data
all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))
all$SocGrade_AB <- all$SocGrade_AB * 100
all$SocGrade_C1 <- all$SocGrade_C1 * 100
all$SocGrade_C2 <- all$SocGrade_C2 * 100
all$SocGrade_DE <- all$SocGrade_DE * 100

names(all) <- gsub(".","_", names(all), fixed = TRUE)
names(all) <- gsub(" ","_", names(all), fixed = TRUE)

classf <- readRDS("data-prepared/area_classification.Rds")
classf <- classf[,c("SOA Code","Supergroup Name","Group Code","Group Name")]
names(classf) <- c("LSOA11","supergroup_class","group_code","group_class")
classf$group_class <- paste0(classf$group_code," ", classf$group_class)
classf <- classf[,c("LSOA11","supergroup_class","group_class")]
all <- left_join(all, classf, by = "LSOA11")
rm(classf)

summary(all$Age_Median)
summary(all$Retired)

all$Age65plus <- all$Age65to74 + all$Age75to84 + all$Age85to89 + all$Age90plus
all$gas_kwh_percap <- all$TotDomGas_11_kWh / all$pop2011


all_old <- all[all$Age65plus > 21.28,]
require(scales)

ggplot(all, aes(median_household_income, gas_kwh_percap,colour = Age65plus > 30)) +
  geom_point(size = 0.1, shape = 15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ylab("Gas consumption per capita (kWh)") +
  xlab("Median Household Income (GBP)") +
  labs(color="Over 30% residents aged 65 plus") +
  theme(legend.position="top") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggsave("plots/gas_old_age.png", width = 6, height = 3, 
         dpi = 600, scale = 1.8)
  
ggplot(all, aes(median_household_income, all$MeanDomGas_11_kWh ,colour = Age65plus > 30)) +
  geom_point(size = 0.1, shape = 15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ylab("Gas consumption per household (kWh)") +
  xlab("Median Household Income (GBP)") +
  labs(color="Over 30% residents aged 65 plus") +
  theme(legend.position="top") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggsave("plots/gas_old_age_hh.png", width = 6, height = 3, 
         dpi = 600, scale = 1.8)

summary(all$Age65plus > 30 & !is.na(all$MeanDomGas_11_kWh))
summary(all$Age65plus[is.na(all$MeanDomGas_11_kWh)])

bounds <- 
