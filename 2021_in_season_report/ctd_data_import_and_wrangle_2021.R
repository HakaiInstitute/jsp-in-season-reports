#DATA IMPORT
library(tidyverse)
library(lubridate)
library(here)

current_year <- year(Sys.Date())

# Get CTD data from EIMS database using R API

# Run this line independently, check console for URL, and authorize
client <- hakaiApi::Client$new()

qu39_all <- 
  client$get(paste0(client$api_root,
                    "/ctd/views/file/cast/data?fields=start_dt,station,conductivity,temperature,depth,salinity,dissolved_oxygen_ml_l&start_dt.year>2014&station=QU39&start_dt.doy>32&start_dt.doy<213&depth<=30&limit=-1")) %>% mutate(
    year = year(start_dt),
    date = as_date(start_dt),
    yday = yday(start_dt),
    week = week(start_dt)
  )


## Create time series of average conditions which includes the current year, using a loess function
qu39_average <- qu39_all %>% 
  filter(year != current_year) %>% 
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
  group_by(date, yday) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = T),
    mean_do = mean(dissolved_oxygen_ml_l, na.rm = T),
    mean_salinity = mean(salinity, na.rm = T)
  )

## Create current year data to compare to time series
qu39_this_year <- qu39_all %>%
  filter(year == current_year) %>%
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

write_csv(qu39_this_year, here::here("2021_in_season_report", "data", "qu39_this_year.csv"))

## SST ANOMALY DATA

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
  drop_na(diff) %>% #  Run code up to here to identify intersections and then add rows
  add_row(
    station = "QU39",
    yday = (39 + 41) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (39 + 41) / 2),
    mean_temp = predict(temp.lo_qu39, (39 + 41) / 2)
  ) %>%
  add_row(
    station = "QU39",
    yday = 52,
    predicted_mean_temp = predict(temp.lo_qu39, 52),
    mean_temp = predict(temp.lo_qu39, 52)
  ) %>%
  add_row(
    station = "QU39",
    yday = (140 + 148) / 2,
    predicted_mean_temp = predict(temp.lo_qu39, (140 + 148) / 2),
    mean_temp = predict(temp.lo_qu39, (140 + 148) / 2)
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

average_temps <- qu39_average

write_csv(average_temps, here::here("2021_in_season_report", "data", "average_temps.csv"))

temperature_anomaly_data <- qu39_temp_anomaly_data

write_csv(temperature_anomaly_data,
          here::here("2021_in_season_report", "data", "temperature_anomaly_data.csv"))
