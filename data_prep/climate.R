
# install.packages("devtools")
# devtools::install_github("mikejohnson51/climateR")

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(climateR)
library(exactextractr)

library(leaflet)
library(plotly)

source("gdd.R")

# Create grid to match AgWeather ----

MIN_LAT = 42.4
MAX_LAT = 47.1
MIN_LNG = -93.0
MAX_LNG = -86.8
MAX_YR = year(Sys.Date()) - 1
MIN_YR = MAX_YR - 10
WEATHER_RDS = str_glue("weather-{MIN_YR}-{MAX_YR}.rds")

# convert points to 0.1 decimal degree grids
# need to define each corner of the grid and repeat the first corner
points_to_grid <- function(lat, lng, d = .05) {
  st_polygon(list(rbind(
    c(lng - d, lat + d),
    c(lng + d, lat + d),
    c(lng + d, lat - d),
    c(lng - d, lat - d),
    c(lng - d, lat + d)
  )))
}

wi_grid <- tibble(lat = seq(MIN_LAT, MAX_LAT, .1)) %>%
  reframe(lng = seq(MIN_LNG, MAX_LNG, .1), .by = lat) %>%
  mutate(geometry = st_sfc(points_to_grid(lat, lng), crs = 4326), .by = c(lat, lng)) %>%
  st_set_geometry("geometry")

wi_grid %>% write_sf("wi_grid.geojson", delete_dsn = T)

wi_boundary <- wi_grid %>% st_bbox() %>% st_as_sfc()

# should show checkerboard pattern to check for correct grid size
wi_grid %>%
  mutate(row = row_number()) %>%
  filter(row %% 2 == 0) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = wi_boundary, weight = 1, color = "black", fillOpacity = 0) %>%
  addPolygons(weight = .1, label = ~str_glue("{lat}, {lng}"))



# Download GridMET ----

# units in Kelvin
gridMET <- getGridMET(
  wi_grid,
  varname = c("tmmn", "tmmx"),
  startDate = str_glue("{MAX_YR - 10}-1-1"),
  endDate = str_glue("{MAX_YR}-12-31")
)

# convert K to C
tmmn <- gridMET$daily_minimum_temperature - 273.15
tmmx <- gridMET$daily_maximum_temperature - 273.15

# save or restore data
tmmn %>% saveRDS("gridmet_tmmn.rds")
tmmx %>% saveRDS("gridmet_tmmx.rds")
tmmn <- readRDS("gridmet_tmmn.rds")
tmmx <- readRDS("gridmet_tmmx.rds")



# extract gridMET data ----

# extract the 4km grids from gridMET into the .1 degree grids for AgWeather
extr_tmmn <- exact_extract(tmmn, wi_grid, "mean")
extr_tmmx <- exact_extract(tmmx, wi_grid, "mean")

# convert to long form
min_temps <- wi_grid %>%
  st_drop_geometry() %>%
  bind_cols(extr_tmmn) %>%
  pivot_longer(
    starts_with("mean"),
    names_to = "date",
    names_transform = ~str_replace(.x, "mean.tmmn_", ""),
    values_to = "min_temp"
  )

# convert to long form
max_temps <- wi_grid %>%
  st_drop_geometry() %>%
  bind_cols(extr_tmmx) %>%
  pivot_longer(
    starts_with("mean"),
    names_to = "date",
    names_transform = ~str_replace(.x, "mean.tmmx_", ""),
    values_to = "max_temp"
  )

# join and fix temps
temps <- min_temps %>%
  left_join(max_temps) %>%
  mutate(
    min = mapply(min, min_temp, max_temp),
    max = mapply(max, min_temp, max_temp)
  ) %>%
  select(lat, lng, date, min_temp = min, max_temp = max) %>%
  mutate(date = as_date(date))

# should show no rows
temps %>% filter(min_temp > max_temp)

# create weather dataset for alfalfa tool
weather <- temps %>%
  drop_na(min_temp, max_temp) %>%
  mutate(across(c(lat, lng), ~round(.x, 1))) %>%
  mutate(
    year = year(date),
    yday = yday(date),
    .after = date
  ) %>%
  mutate(
    min_temp = min_temp * 1.8 + 32,
    max_temp = max_temp * 1.8 + 32,
    frost = min_temp < 32,
    freeze = min_temp < 28,
    gdd41 = calc_gdd(min_temp, max_temp, 41, 86),
    gdd50 = calc_gdd(min_temp, max_temp, 50, 86)
  ) %>%
  arrange(lat, lng, date)

# save or restore data
rm(list = c("extr_tmmn", "extr_tmmx", "min_temps", "max_temps"))
weather %>% write_rds(WEATHER_RDS, "gz")
weather <- read_rds(WEATHER_RDS)



# generate climate statistics ----


# TODO: add cumulative frost/freeze probabilities looking out from summer

summarize_climate <- function(df) {
  df %>%
    summarize(
      across(c(min_temp, max_temp, gdd41, gdd50), ~mean(.x, na.rm = T)),
      across(c(frost, freeze), ~ sum(.x, na.rm = T) / n()),
      .by = c(lat, lng, yday)
    )
}

climate_10yr <- summarize_climate(weather)
climate_5yr <- weather %>%
  filter(year >= MAX_YR - 5) %>%
  summarize_climate()


# add cumulative prob of frost after date / no frost til date

add_cumul_probs <- function(df) {
  spring <- df %>%
    filter(yday < 200) %>%
    arrange(lat, lng, desc(yday)) %>%
    mutate(
      frost_by = 1 - cumprod(1 - frost),
      freeze_by = 1 - cumprod(1 - freeze),
      .by = c(lat, lng)
    )

  fall <- df %>%
    filter(yday >= 200) %>%
    arrange(lat, lng, yday) %>%
    mutate(
      frost_by = 1 - cumprod(1 - frost),
      freeze_by = 1 - cumprod(1 - freeze),
      .by = c(lat, lng)
    )

  bind_rows(spring, fall) %>%
    arrange(lat, lng, yday)
}

climate <- list(
  c5 = climate_5yr %>% add_cumul_probs(),
  c10 = climate_10yr %>% add_cumul_probs()
)

# save/restore climate data
climate %>% write_rds("climate.rds", "gz")
climate <- read_rds("climate.rds")




# inspect climate data

climate_10yr %>%
  filter(yday == 150) %>%
  rename(value = max_temp) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRectangles(
    lat1 = ~lat - .05,
    lat2 = ~lat + .05,
    lng1 = ~lng - .05,
    lng2 = ~lng + .05,
    weight = .5,
    opacity = .1,
    color = "black",
    fillOpacity = .75,
    fillColor = ~colorNumeric("viridis", value)(value),
    label = ~round(value, 1)
  )

climate_10yr %>%
  filter(lat == min(lat), lng == min(lng)) %>%
  ggplot(aes(x = yday, y = gdd41)) +
  geom_line() +
  geom_smooth(method = "gam")

climate_10yr %>%
  filter(lat == min(lat), lng == min(lng)) %>%
  ggplot(aes(x = yday, y = freeze)) +
  geom_line() +
  geom_smooth(method = "gam")

climate_10yr %>%
  filter(lat == min(lat), lng == min(lng)) %>%
  filter(yday > 200) %>%
  mutate(frost_by = 1 - cumprod(1 - frost)) %>%
  mutate(freeze_by = 1 - cumprod(1 - freeze)) %>%
  mutate(date = as_date("2024-1-1") + yday - 1) %>%
  plot_ly(x = ~date) %>%
  add_lines(y = ~frost_by, name = "Prob of frost by date") %>%
  add_lines(y = ~freeze_by, name = "Prob of freezing by date")

climate %>%
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) %>%
  mutate(date = as_date("2024-1-1") + yday - 1) %>%
  plot_ly(x = ~date) %>%
  add_lines(y = ~frost_by, name = "Prob of frost by date") %>%
  add_lines(y = ~freeze_by, name = "Prob of freezing by date")
