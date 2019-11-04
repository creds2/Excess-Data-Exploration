library(tidytransit)

gtfs <- tidytransit::read_gtfs("https://github.com/ITSLeeds/UK2GTFS/releases/download/0.001/EA_redo.zip")
validation_result <- attr(gtfs, "validation_result")

# stop_freq <- get_stop_frequency(gtfs, start_hour = 0L, end_hour = 24L,
#                                 by_route = TRUE)

make_trip_geoms <- function(gtfs){
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  #routes <- gtfs$routes
  trips <- gtfs$trips
  #calendar <- gtfs$calendar
  
  stops <- stops[,c("stop_id","stop_lon","stop_lat")]
  stop_times <- dplyr::left_join(stop_times, stops, by = "stop_id")
  
  
  stop_times <- split(stop_times, stop_times$trip_id)
  
  make_geom <- function(x){
    geom <- x[,c("stop_lon", "stop_lat")]
    geom <- as.matrix(geom)
    geom <- sf::st_linestring(geom, dim = "XY")
    return(geom)
  }
  
  geom <- pbapply::pblapply(stop_times, make_geom)
  
  trips_geom <- sf::st_as_sf(data.frame(trip_id = names(stop_times),
                           geometry = sf::st_sfc(geom, crs = 4326),
                            stringsAsFactors = FALSE))
  
  return(trips_geom)
  
}

trips_geom <- make_trip_geoms(gtfs)
trips_geom$length_km <- as.numeric(st_length(trips_geom)) / 1000
# trips_geom$ntrip <- 1
# trips_overline <- stplanr::overline2(trips_geom, "ntrip")
# tm_shape(trips_overline) +
#   tm_lines(col = "ntrip",
#            lwd = 2,
#            breaks = c(0,1,10,20,30,40,50,60,70,80,100,1000,1600))


trips_local_id <- trips_geom$trip_id[trips_geom$length_km < 50]
trips_ld_id <- trips_geom$trip_id[trips_geom$length_km >= 50]

# Work out total stops per day
period_start <- min(calendar$start_date)
period_end <- max(calendar$end_date)
period_end - period_start


countwd2 <- function(startdate, enddate, weekday){
  d <- as.integer(enddate - startdate) + 1
  d %/% 7 +
    (weekday %in% weekdays(seq(startdate, length.out=d %% 7, by=1)))
}

count_stops <- function(gtfs){
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- gtfs$calendar
  calendar_days <- gtfs$calendar_dates
  
  calendar_days <- calendar_days %>%
    group_by(service_id) %>%
    summarise(runs_extra = sum(exception_type == 1),
              runs_canceled = sum(exception_type == 2))
  
  # work out how many times the trip in run
  trips <- dplyr::left_join(trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_days, by = "service_id")
  trips$runs_canceled[is.na(trips$runs_canceled)] <- 0
  trips$runs_extra[is.na(trips$runs_extra)] <- 0
  
  trips$n_monday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Monday")
  trips$n_tuesday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Tuesday")
  trips$n_wednesday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Wednesday")
  trips$n_thursday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Thursday")
  trips$n_friday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Friday")
  trips$n_saturday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Saturday")
  trips$n_sunday <- mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Sunday")
  
  trips$runs_monday <- trips$monday * trips$n_monday
  trips$runs_tuesday <- trips$tuesday * trips$n_tuesday
  trips$runs_wednesday <- trips$wednesday * trips$n_wednesday
  trips$runs_thursday <- trips$thursday * trips$n_thursday
  trips$runs_friday <- trips$friday * trips$n_friday
  trips$runs_saturday <- trips$saturday * trips$n_saturday
  trips$runs_sunday <- trips$sunday * trips$n_sunday
  
  trips$runs_total <- trips$runs_monday + trips$runs_tuesday + 
    trips$runs_wednesday + trips$runs_thursday + trips$runs_friday + 
    trips$runs_saturday + trips$runs_sunday + trips$runs_extra - trips$runs_canceled
  
  trips$runs_per_week <- trips$runs_total / (as.numeric(max(trips$end_date) - min(trips$start_date))/7)
  
  trips <- trips[,c("trip_id","start_date","end_date","runs_total","runs_per_week")]
  stop_times <- dplyr::left_join(stop_times, trips, by = "trip_id")
  stop_times_summary <- stop_times %>%
    group_by(stop_id) %>%
    summarise(stops_total = sum(runs_total),
              stops_per_week = sum(runs_per_week))
  
  
  foo = left_join(stops, stop_times_summary, by = "stop_id")
  foo = st_as_sf(foo, coords = c("stop_lon","stop_lat"), crs = 4326)
  tm_shape(foo) +
    tm_dots(col = "stops_per_week",
            breaks = quantile(foo$stops_per_week, probs = c(seq(0,1,0.1))))
  
  #countwd2(trips$start_date[1], trips$end_date[1], "Monday")
  
}

bounds <- read_sf("data-prepared/LSOA_generalised.gpkg")
bounds <- st_transform(bounds, 27700)
foo <- st_transform(foo, 27700)
foo2 <- st_join(foo, bounds)
foo2 <- st_drop_geometry(foo2)
foo2 <- foo2 %>%
  group_by(LSOA11) %>%
  summarise(stops_total = sum(stops_total),
            stops_per_week = sum(stops_per_week))

bounds2 <- left_join(bounds, foo2, by = "LSOA11")
bounds2 <- bounds2[!is.na(bounds2$stops_total),]
tm_shape(bounds2) +
  tm_fill(col = "stops_per_week",
          breaks = quantile(bounds2$stops_per_week, probs = c(seq(0,1,0.1))))

# foo = trips_geom[!duplicated(trips_geom$geometry),]
# qtm(foo, lines.col = "length_km")
#trip_join <- trips[,c("route_id","trip_id")]
#trip_join <- unique(trip_join)
# stop_times <- dplyr::left_join(stop_times, trip_join, by = "trip_id")
# stop_times_summary <- stop_times %>%
#   group_by(trip_id) %>%
#   summarise(stops = paste(stop_id, collapse = " "))
# summary(duplicated(stop_times_summary$trip_id))
# summary(duplicated(stop_times_summary$stops))
