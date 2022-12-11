# Final Version of the Analysis - Redone for Emissions

# Libraries and Setup
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(grid)

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/Data/CREDS Data"
} else if(dir.exists("E:/OneDrive - University of Leeds/Data/CREDS Data")){
  secure_path <- "E:/OneDrive - University of Leeds/Data/CREDS Data"
} else {
  secure_path <- "D:/OneDrive - University of Leeds/Data/CREDS Data"
}

# LSOA bounds
bounds = read_sf("data-prepared/LSOA_forplots.gpkg")
bounds$country = substr(bounds$LSOA11, 1, 1)
bounds = bounds[bounds$country %in% c("E"),]

# Get Region Bounds
la <- readRDS("data-prepared/la_bounds.Rds")
la <- la[!la$la %in% c("Blaenau Gwent",
                       "Bridgend",
                       "Cardiff",
                       "Carmarthenshire",
                       "Caerphilly",
                       "Ceredigion",
                       "Conwy",
                       "Denbighshire",
                       "Flintshire",
                       "Gwynedd",
                       "Isle of Anglesey",
                       "Merthyr Tydfil",
                       "Monmouthshire",
                       "Neath Port Talbot",
                       "Newport",
                       "Pembrokeshire",
                       "Powys",
                       "Rhondda Cynon",
                       "Swansea",
                       "Torfaen",
                       "Wrexham",
                       "The Vale of Glamorgan",
                       "Rhondda Cynon Taf"),]


all <- readRDS(paste0(secure_path,"/github-secure-data/lsoa_all2.Rds"))
all <- all[substr(all$LSOA11,1,1) == "E",]
emissions <- readRDS("../CarbonCalculator/data-prepared/car_historical_emissions.Rds")
emissions <- emissions[,c("LSOA","AvgCO2_cars_2011")]
all <- left_join(all, emissions, by = c("LSOA11" = "LSOA"))

all$car_km_11 = all$cars_miles * 1.60934
all$car_kgco2_per_cap = all$car_km_11 * (all$AvgCO2_cars_2011 / 1000) / all$pop2011

# Plot General Distribution of ENergy Use
bounds <- left_join(bounds, all[,c("LSOA11","car_kgco2_per_cap")])
bounds$country <- substr(bounds$LSOA11,1,1)
bounds <- bounds[bounds$country == "E",]

#Plot Driving Emissions
tmap_mode("plot")
map2 <- tm_shape(bounds[substr(bounds$LSOA11,1,1) == "E",]) +
  tm_fill(c("car_kgco2_per_cap"),
          palette = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027"),
          breaks = c(0,500,600,700,800,900,1000,1100,1200,4000),
          title = "") +
  tm_layout(legend.position = c("right","top"),
            outer.margins=0) +
  tm_shape(la) +
  tm_layout(frame = FALSE) +
  tm_borders("grey") +
  tm_compass() +
  tm_scale_bar()

# Inset for London
la_lnd <- la[la$la %in% c("Hillingdon","Hounslow","Richmond upon Thames","Kingston upon Thames","Sutton","Croydon","Bromley",
                          "Bexley","Havering","Barking and Dagenham","Redbridge","Waltham Forest","Enfield","Haringey",
                          "Barnet","Harrow","Brent","Ealing","Hammersmith and Fulham","Kensington and Chelsea","Westminster",
                          "Camden","Islington","Hackney","Tower Hamlets","City of London","Greenwich","Lewisham","Southwark",
                          "Lambeth","Wandsworth","Merton","Newham"),]

lnd <- st_union(la_lnd)
lnd <- st_buffer(lnd, 10)

lsoa_lnd <- bounds[lnd,, op = st_within]

map_lnd <- tm_shape(lsoa_lnd) +
  tm_fill(c("car_kgco2_per_cap"),
          palette = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027"),
          breaks = c(0,500,600,700,800,900,1000,1100,1200,4000),
          legend.show = FALSE) +
  tm_layout(frame = FALSE, outer.margins=0, bg.color = "transparent") +
  tm_shape(la_lnd) +
  tm_borders("grey")

# Inset for Manchester
la_mcr <- la[la$la %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford","Wigan"),]

mcr <- st_union(la_mcr)
mcr <- st_buffer(mcr, 10)

lsoa_mcr <- bounds[mcr,, op = st_within]

map_mcr <- tm_shape(lsoa_mcr) +
  tm_fill(c("car_kgco2_per_cap"),
          palette = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027"),
          breaks = c(0,500,600,700,800,900,1000,1100,1200,4000),
          legend.show = FALSE) +
  tm_layout(frame = FALSE, outer.margins=0, bg.color = "transparent") +
  tm_shape(la_mcr) +
  tm_borders("grey")

# Inset for WM


la_wm <- la[la$la %in% c("Birmingham","Wolverhampton","Coventry","Dudley","Sandwell","Solihull","Walsall"),]

wm <- st_union(la_wm)
wm <- st_buffer(wm, 10)

lsoa_wm <- bounds[wm,, op = st_within]

map_wm <- tm_shape(lsoa_wm) +
  tm_fill(c("car_kgco2_per_cap"),
          palette = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027"),
          breaks = c(0,500,600,700,800,900,1000,1100,1200,4000),
          legend.show = FALSE) +
  tm_layout(frame = FALSE, outer.margins=0, bg.color = "transparent") +
  tm_shape(la_wm) +
  tm_borders("grey")





xy <- st_bbox(la)
asp <- (xy$ymax - xy$ymin)/(xy$xmax - xy$xmin)
xy <- st_bbox(lsoa_lnd)
asp_lnd <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)
xy <- st_bbox(lsoa_mcr)
asp_mcr <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)
xy <- st_bbox(lsoa_wm)
asp_wm <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)
w <- 0.39 # Widht of inset
h_lnd <-  w / asp_lnd 
h_mcr <-  w / asp_mcr
h_wm <-  w / asp_wm
vp_lnd <- viewport(x=0.39, y=0.5, width = w, height=h_lnd, just=c("right", "top"))
vp_mcr <- viewport(x=0.39, y=1.0, width = w, height=h_mcr, just=c("right", "top"))
vp_wm <- viewport(x=0.39, y=0.75, width = w, height=h_wm, just=c("right", "top"))

tmap_save(map2,filename="plots/emissions_driving_inset.png",
          dpi=300, 
          insets_tm=list(map_lnd,map_mcr,map_wm), 
          insets_vp=list(vp_lnd, vp_mcr, vp_wm),
          height=asp*200, width=200, units="mm")

#tmap_save(map2,"plots/emissions_driving.png")


box = all[,c("LSOA11","car_kgco2_per_cap")]

classf <- readRDS("data-prepared/area_classification.Rds")
classf <- classf[,c("SOA Code","Supergroup Name","Group Code","Group Name")]
names(classf) <- c("LSOA11","supergroup_class","group_code","group_class")
classf$group_class <- paste0(classf$group_code," ", classf$group_class)
classf <- classf[,c("LSOA11","supergroup_class","group_class")]
box <- left_join(box, classf, by = "LSOA11")
rm(classf)

lvls2 <- box %>% 
  group_by(group_class) %>%
  summarise(mean = median(car_kgco2_per_cap, na.rm = TRUE))
lvls2 <- lvls2[order(lvls2$mean),]
lvls2 <- lvls2$group_class

# lvls2 <- c("6a Inner city cosmopolitan","4d Hard-pressed flat dwellers",
#            "1a Cosmopolitan student neighbourhoods", "7b Young ethnic communities",
#            "4a Challenged white communities","7a Urban cultural mix",
#            "4c Hampered neighbourhoods","4b Constrained renters",
#            "5d Endeavouring social renters","3d Households in terraces and flats",
#            "5e Primary sector workers","5a Ageing urban communities",
#            "3c Highly qualified professionals","5b Aspiring urban households",
#            "5c Comfortable neighbourhoods","3b Asian traits",
#            "8b Ageing suburbanites",
#            "3a Achieving neighbourhoods","8c Comfortable suburbia",
#            "8a Affluent communities",
#            "2d Rural traits",
#            "2a Ageing rural neighbourhoods",
#            "2c Remoter communities",
#            "2b Prospering countryside life")

box$labs <- factor(box$group_class,
                         levels = lvls2 ,ordered = TRUE)

plot1 <- ggplot(box, aes(labs, car_kgco2_per_cap, fill = supergroup_class )) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 2100), expand = c(0, 0)) +
  coord_flip() +
  ylab(expression("Driving annual emissions (kgCO"[2]*"/capita)")) +
  xlab("Area Classification") +
  theme(legend.position = "none")
ggsave("plots/box_emissions_driving.png", plot1, width = 6, height = 4, 
       dpi = 600, scale = 1.8)



