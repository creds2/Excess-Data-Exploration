# quick test of new clusters
library(dplyr)
library(reshape2)
library(ggplot2)

# Input Data --------------------------------------------------------------
dir.create("temp")
unzip("../Secure-Data/Excess/XSExpData1.zip", exdir = "temp")
lsoa <- read.csv("temp/XSExpData1.csv")
unlink("temp", recursive = T)

modium <- readRDS("data-prepared/modium.Rds")
epc <- readRDS("data-prepared/epc.Rds")

lsoa <- left_join(lsoa, modium, by = c("LSOA11CD" = "LSOA11"))
lsoa <- left_join(lsoa, epc, by = c("LSOA11CD" = "LSOA11"))

lsoa <- lsoa[,c("LSOA11CD","nrgHH","gasAv","elecAv","all","Dwellings","Crr_EE","Cluster_mode")]

ggplot(lsoa, aes(Crr_EE, gasAv)) +
  geom_point()

ggplot(lsoa, aes(Crr_EE, elecAv)) +
  geom_point()


ggplot(lsoa, aes(Cluster_mode, gasAv)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(lsoa, aes(Cluster_mode, nrgHH)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(lsoa, aes(Cluster_mode, elecAv)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(lsoa, aes(Cluster_mode, all)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1))



