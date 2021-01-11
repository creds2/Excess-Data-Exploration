dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/CREDS Data/github-secure-data/Historical_Car_Emissions_LSOA.zip",
      exdir = "tmp")
dat <- read.csv("tmp/Historical_Car_Emissions_LSOA.csv")
unlink("tmp", recursive = TRUE)

head(dat)
unique(substr(dat$LSOA,1,1))

dat$country <- substr(dat$LSOA,1,1)

library(dplyr)

summ <- dat %>%
  group_by(country, year, fuel) %>%
  summarise(cars = sum(AllCars, na.rm = TRUE),
            AvgCO2_mean = mean(AvgCO2, na.rm = TRUE),
            AvgAge_mean = mean(AvgAge, na.rm = TRUE))

summ <- summ[summ$country %in% c("E","W","S"),]

library(ggplot2)

ggplot(summ, aes(x = year, y = cars, fill = fuel)) +
  geom_col() +
  scale_x_continuous(breaks=seq(2001,2018,2)) + 
  facet_grid(rows = vars(country)) +
  ggtitle("Number of Cars by Year and Fuel Type by Country (excluding between owners)")

ggplot(summ, aes(x = year, y = AvgCO2_mean, colour = fuel)) +
  geom_line() +
  scale_x_continuous(breaks=seq(2001,2018,2)) +
  facet_grid(rows = vars(country)) +
  ggtitle("Mean CO2 of Cars (by LSOAs) for Year and Fuel Type by Country (excluding between owners)")

ggplot(summ, aes(x = year, y = AvgAge_mean, colour = fuel)) +
  geom_line() +
  scale_x_continuous(breaks=seq(2001,2018,2)) +
  facet_grid(rows = vars(country)) +
  ggtitle("Mean Age of Cars (by LSOAs) for Year and Fuel Type by Country (excluding between owners)")

ggplot(summ, aes(x = year, y = cars, fill = fuel)) +
  geom_bar(position="fill", stat="identity") +
  scale_x_continuous(breaks=seq(2001,2018,2)) + 
  facet_grid(rows = vars(country)) +
  ggtitle("% of Cars by Year and Fuel Type by Country (excluding between owners)")


write.csv(summ,"data-output/country_car_summary.csv", row.names = FALSE)
