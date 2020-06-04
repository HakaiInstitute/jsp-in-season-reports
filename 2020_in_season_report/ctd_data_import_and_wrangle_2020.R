#DATA IMPORT
library(tidyverse)
library(lubridate)
library(here)

current_year <- year(Sys.Date())

# Get CTD data from EIMS database using R API

# Run this line independently, check console for URL, and authorize
client <- hakaiApi::Client$new()

qu39_endpoint <-
  sprintf("%s/%s",
          client$api_root,
          "ctd/views/file/cast/data?station=QU39&limit=-1")

qu39_all <- client$get(qu39_endpoint) %>%
  mutate(
    year = year(start_dt),
    date = as_date(start_dt),
    yday = yday(start_dt)
  )

qu29_endpoint <-
  sprintf("%s/%s",
          client$api_root,
          "ctd/views/file/cast/data?station=QU29&limit=-1")

qu29_all <- client$get(qu29_endpoint) %>%
  mutate(
    year = year(start_dt),
    date = as_date(start_dt),
    yday = yday(start_dt)
  )

## Create time series of average conditions which includes the current year, using a loess function

ctd_all <- rbind(qu39_all, qu29_all) %>%
  mutate(
    year = year(start_dt),
    date = as_date(start_dt),
    yday = yday(start_dt),
    week = week(start_dt)
  ) %>%
  filter(year != current_year, depth <= 30) %>%
  select(
    year,
    date,
    week,
    yday,
    station,
    conductivity,
    temperature,
    depth,
    salinity,
    dissolved_oxygen_ml_l
  ) %>%
  group_by(station, date, yday) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = T),
    mean_do = mean(dissolved_oxygen_ml_l, na.rm = T),
    mean_salinity = mean(salinity, na.rm = T)
  )

## Create current year data to compare to time series
ctd_post_time_series <- rbind(qu39_all, qu29_all) %>%
  filter(year == current_year, yday > 32, yday < 213,  depth <= 30) %>%
  select(
    year,
    date,
    yday,
    station,
    conductivity,
    temperature,
    depth,
    salinity,
    dissolved_oxygen_ml_l
  ) %>%
  group_by(station, yday) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = T),
    mean_do = mean(dissolved_oxygen_ml_l, na.rm = T),
    mean_salinity = mean(salinity, na.rm = T)
  )

## SST ANOMALY DATA
## QU39
qu39_average <- ctd_all %>%
  filter(station == "QU39")

# Filter down to station of interest
qu39_this_year <- ctd_post_time_series %>%
  filter(station == "QU39")

write_csv(qu39_this_year, here::here("2020_in_season_report", "data", "qu39_this_year.csv"))

temp.lo_qu39 <-
  loess(mean_temp ~ yday, qu39_average, SE = T, span = 0.35)

#create table for predicitions from loess function
sim_temp_data_qu39 <-
  tibble(yday = seq(min(qu39_average$yday), max(qu39_average$yday), 0.1))
#Predict temp in 0.1 day increments to provide smooth points to join
sim_temp_data_qu39$predicted_mean_temp <-
  predict(temp.lo_qu39, sim_temp_data_qu39, SE = T)


# Create a linear interpolation of points that have zero difference between
# loess model and 'observed data' so that an area plot will look right
# manually identify intersections and create values that fall on the line so that colour of plot will change above and below the trend line.
# This code will have to be re-written manually to interpolate the mid points of when current year points cross over the trendline
# See https://stackoverflow.com/questions/27135962/how-to-fill-geom-polygon-with-different-colors-above-and-below-y-0
qu39_temp_anomaly_data <-
  left_join(sim_temp_data_qu39, qu39_this_year) %>%
  mutate(diff = if_else(mean_temp > predicted_mean_temp, "pos", "neg")) %>%
  drop_na(diff) %>%
  add_row(
    station = "QU39",
    yday = (36 + 43) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (36 + 43) / 2),
    mean_temp = predict(temp.lo_qu39, (36 + 43) / 2)
  ) %>%
  add_row(
    station = "QU39",
    yday = (129 + 142) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (129 + 142) / 2),
    mean_temp = predict(temp.lo_qu39, (129 + 142) / 2)
  ) %>%
  add_row(
    station = "QU39",
    yday = (142 + 149) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (142 + 149) / 2),
    mean_temp = predict(temp.lo_qu39, (142 + 149) / 2)
  ) %>%
  add_row(
    station = "QU39",
    yday = (155 + 149) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (155 + 149) / 2),
    mean_temp = predict(temp.lo_qu39, (155 + 149) / 2)
  ) %>% 
  add_row(
    station = "QU39",
    yday = (176 + 184) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (176 + 184) / 2),
    mean_temp = predict(temp.lo_qu39, (176 + 184) / 2)
  )


# Create min and max for any given day of the time series
qu39_min_max <- qu39_all %>%
  filter(depth <= 30) %>%
  group_by(year, yday) %>%
  summarise(mean_temp = mean(temperature)) %>%
  ungroup() %>%
  group_by(yday) %>%
  summarise(min_temp = min(mean_temp),
            max_temp = max(mean_temp)) %>%
  mutate(station = "QU39")

##QU29
qu29_average <- ctd_all %>%
  filter(station == "QU29")

# Filter down to station of interest
qu29_this_year <- ctd_post_time_series %>%
  filter(station == "QU29")

temp.lo_qu29 <- loess(mean_temp ~ yday, qu29_average, span = 0.65)

#create table for predicitions from loess function
sim_temp_data_qu29 <-
  tibble(yday = seq(min(qu29_average$yday), max(qu29_average$yday), 0.1))
#Predict temp in 0.1 day increments to provide smooth points to join
sim_temp_data_qu29$predicted_mean_temp <-
  predict(temp.lo_qu29, sim_temp_data_qu29, SE = T)

# Create a linear interpolation of points that have zero difference between
# loess model and 'observed data' so that an area plot will look right
# manually identify intersections and create values that fall on the line
qu29_temp_anomaly_data <-
  left_join(sim_temp_data_qu29, qu29_this_year) %>%
  mutate(diff = if_else(mean_temp > predicted_mean_temp, "pos", "neg")) %>%
  drop_na(diff) %>%
  add_row(
    station = "QU29",
    yday = (51 + 36) / 2,
    predicted_mean_temp = predict(temp.lo_qu29, (51 + 36) / 2),
    mean_temp = predict(temp.lo_qu29, (51 + 36) / 2)
  ) %>%
  add_row(
    station = "QU29",
    yday = (157 + 136) / 2,
    predicted_mean_temp = predict(temp.lo_qu29, (157 + 136) / 2),
    mean_temp = predict(temp.lo_qu29, (157 + 136) / 2)
  ) %>%
  add_row(
    station = "QU29",
    yday = (157 + 201) / 2,
    predicted_mean_temp = predict(temp.lo_qu29, (157 + 201) / 2),
    mean_temp = predict(temp.lo_qu29, (157 + 201) / 2)
  )
# Create min and max for any given day of the time series

qu29_min_max <- qu29_all %>%
  filter(depth <= 30) %>%
  group_by(year, yday) %>%
  summarise(mean_temp = mean(temperature)) %>%
  ungroup() %>%
  group_by(yday) %>%
  summarise(min_temp = min(mean_temp),
            max_temp = max(mean_temp)) %>%
  mutate(station = "QU29")

min_max_data <- rbind(qu39_min_max, qu29_min_max)

average_temps <- rbind(qu39_average, qu29_average)

write_csv(average_temps, here::here("2020_in_season_report", "data", "average_temps.csv"))

temperature_anomaly_data <-
  rbind(qu29_temp_anomaly_data,
        qu39_temp_anomaly_data)

write_csv(temperature_anomaly_data,
          here::here("2020_in_season_report", "data", "temperature_anomaly_data.csv"))
