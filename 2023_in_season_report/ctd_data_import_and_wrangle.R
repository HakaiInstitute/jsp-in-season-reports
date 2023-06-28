#DATA IMPORT
library(tidyverse)
library(lubridate)
library(here)


current_year <- year(Sys.Date())

# Get CTD data from EIMS database using R API

qu39_all <- 
  client$get(paste0(client$api_root,
                    "/ctd/views/file/cast/data?direction_flag=d&fields=start_dt,station,conductivity,temperature,depth,salinity,dissolved_oxygen_ml_l&start_dt.year>2014&station=QU39&start_dt.doy>32&start_dt.doy<213&depth<=30&limit=-1")) %>% mutate(
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

write_csv(qu39_this_year, here::here(paste0(current_year,"_in_season_report"), 
                                     "data", "qu39_this_year.csv"))

## SST ANOMALY DATA

# The span parameter has to match that listed in geom_smooth in ggplot
temp.lo_qu39 <-
  loess(mean_temp ~ yday, qu39_average, SE = T, span = 0.65)

#create table for predicitions from loess function
sim_temp_data_qu39 <-
  tibble(yday = seq(min(qu39_average$yday), max(qu39_average$yday), 0.1))
#Predict temp in 0.1 day increments to provide smooth points to join
sim_temp_data_qu39$predicted_mean_temp <-
  predict(temp.lo_qu39, sim_temp_data_qu39, SE = T)


# Create a linear interpolation of points that have zero difference between
# loess model and 'observed data' so that an area plot will look right
library(purrr)
# Function to calculate the midpoint row
calc_midpoint_row <- function(df, i) {
  yday = (df$yday[i] + df$yday[i+1]) / 2
  predicted_mean_temp = (df$predicted_mean_temp[i] + 
                           df$predicted_mean_temp[i+1]) / 2
  mean_temp = predicted_mean_temp
  return(data.frame(yday = yday, predicted_mean_temp = predicted_mean_temp, 
                    mean_temp = mean_temp))
}

# Generate the initial data
qu39_temp_anomaly_data <- left_join(sim_temp_data_qu39, qu39_this_year) %>%
  mutate(diff = if_else(mean_temp > predicted_mean_temp, "pos", "neg")) %>%
  drop_na(diff)

# Generate mid-point rows
mid_points <- map_dfr(1:(nrow(qu39_temp_anomaly_data) - 1), ~ {
  if(qu39_temp_anomaly_data$diff[.x] != qu39_temp_anomaly_data$diff[.x + 1]) {
    calc_midpoint_row(qu39_temp_anomaly_data, .x)
  }
})

# Combine original data with the new mid-point rows
qu39_temp_anomaly_data <- bind_rows(qu39_temp_anomaly_data, mid_points) %>%
  arrange(yday)  # make sure data is in order by yday


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

write_csv(average_temps, here::here(paste0(current_year, "_in_season_report"), "data", "average_temps.csv"))

temperature_anomaly_data <- qu39_temp_anomaly_data

write_csv(temperature_anomaly_data,
          here::here(paste0(current_year, "_in_season_report"), "data", "temperature_anomaly_data.csv"))
