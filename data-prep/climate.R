#' Generate climate data for the Alfalfa weather tool

# install.packages("devtools")
# devtools::install_github("mikejohnson51/climateR")

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(climateR)
library(exactextractr)
library(fst)
library(leaflet)
library(plotly)


# Parallel processing
library(foreach)
library(doParallel)

# Set up parallel backend
registerDoParallel(cores = detectCores() - 1)

# Stop parallel backend
# stopImplicitCluster()


#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @returns single sine growing degree days for one day
# gdd_sine <- function(tmin, tmax, base) {
#   mapply(function(tmin, tmax, base) {
#     if (is.na(tmin) || is.na(tmax)) return(NA)
#
#     # swap min and max if in wrong order for some reason
#     if (tmin > tmax) { t = tmin; tmin = tmax; tmax = t }
#
#     # min and max < lower
#     if (tmax <= base) return(0)
#
#     average = (tmin + tmax) / 2
#
#     # tmin > lower = simple average gdds
#     if (tmin >= base) return(average - base)
#
#     # tmin < lower, tmax > lower = sine gdds
#     alpha = (tmax - tmin) / 2
#     base_radians = asin((base - average) / alpha)
#     a = average - base
#     b = pi / 2 - base_radians
#     c = alpha * cos(base_radians)
#     (1 / pi) * (a * b + c)
#   }, tmin, tmax, base)
# }


gdd_sine <- function(tmin, tmax, base, upper = 150) {
  if (base > upper) stop("Base cannot be greater than Upper")

  map2_dbl(tmin, tmax, function(tmin, tmax) {
    if (is.na(tmin) || is.na(tmax)) return(NA)
    if (tmin > tmax) {
      temp = tmin
      tmin = tmax
      tmax = temp
    }

    # min and max > upper
    if (tmin >= upper) return(upper - base)

    # min and max < lower
    if (tmax <= base) return(0)

    average = (tmin + tmax) / 2

    # min and max between base and upper
    if (tmax <= upper && tmin >= base) return(average - base)

    alpha = (tmax - tmin) / 2

    # min < base, max between base and upper
    if (tmax <= upper && tmin < base) {
      base_radians = asin((base - average) / alpha)
      a = average - base
      b = pi / 2 - base_radians
      c = alpha * cos(base_radians)
      return((1 / pi) * (a * b + c))
    }

    # max > upper and min between base and upper
    if (tmax > upper && tmin >= base) {
      upper_radians = asin((upper - average) / alpha)
      a = average - base
      b = upper_radians + pi / 2
      c = upper - base
      d = pi / 2 - upper_radians
      e = alpha * cos(upper_radians)
      return((1 / pi) * (a * b + c * d - e))
    }

    # max > upper and min < base
    if (tmax > upper && tmin < base) {
      base_radians = asin((base - average) / alpha)
      upper_radians = asin((upper - average) / alpha)
      a = average - base
      b = upper_radians - base_radians
      c = alpha * (cos(base_radians) - cos(upper_radians))
      d = upper - base
      e = pi / 2 - upper_radians
      return((1 / pi) * ((a * b + c) + (d * e)))
    }
  })
}

# test gdd function
tibble(
  tmin = runif(100),
  tmax = runif(100),
  gdd = gdd_sine(tmin, tmax, .5, .8),
  cumgdd = cumsum(gdd)
) %>%
  mutate(day = row_number()) %>%
  ggplot(aes(x = day)) +
  geom_col(aes(y = gdd)) +
  geom_line(aes(y = cumgdd))



# settings
OPTS <- lst(
  extent_wi = list(
    lat = c(42.4, 47.1),
    lng = c(-93.0, -86.8)
  ),
  extent_mw = list(
    lat = c(38, 50),
    lng = c(-98, -82)
  ),
  max_yr = 2024,
  yrs_30 = seq(max_yr - 30, max_yr, 1),
  yrs_10 = seq(max_yr - 10, max_yr, 1),
  yrs_5 = seq(max_yr - 5, max_yr, 1)
)


# Create grid to match AgWeather -----------------------------------------------

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

grid_from_extent <- function(extent) {
  lats = seq(extent$lat[1], extent$lat[2], .1)
  lngs = seq(extent$lng[1], extent$lng[2], .1)
  tibble(lat = lats) %>%
    reframe(lng = lngs, .by = lat) %>%
    mutate(geometry = st_sfc(points_to_grid(lat, lng), crs = 4326), .by = c(lat, lng)) %>%
    st_set_geometry("geometry")
}

wi_grid <- grid_from_extent(OPTS$extent_wi)
wi_grid %>% saveRDS("grids/wi_grid.rds")
wi_grid %>% write_sf("grids/wi_grid.geojson", delete_dsn = T)

mw_grid <- grid_from_extent(OPTS$extent_mw)
mw_grid %>% saveRDS("grids/mw_grid.rds")
mw_grid %>% write_sf("grids/mw_grid.geojson", delete_dsn = T)

wi_boundary <- wi_grid %>% st_bbox() %>% st_as_sfc()
mw_boundary <- mw_grid %>% st_bbox() %>% st_as_sfc()

# should show checkerboard pattern to check for correct grid size
wi_grid %>%
  mutate(row = row_number()) %>%
  filter(row %% 2 == 0) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = wi_boundary, weight = 1, color = "black", fillOpacity = 0) %>%
  addPolygons(weight = .1, label = ~str_glue("{lat}, {lng}"))

mw_grid %>%
  mutate(row = row_number()) %>%
  filter(row %% 2 == 0) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = mw_boundary, weight = 1, color = "black", fillOpacity = 0) %>%
  addPolygons(weight = .1, label = ~str_glue("{lat}, {lng}"))



# Download weather data --------------------------------------------------------

layers <- list(
  "tmmn" = "daily_minimum_temperature",
  "tmmx" = "daily_maximum_temperature"
)

lapply(OPTS$yrs, function (yr) {
  fname <- paste0("data/", yr, ".rds")

  if (file.exists(fname)) {
    message(fname, " ==> exists")
    return(fname)
  }

  message(fname, " ==> downloading...")
  gridMET <- getGridMET(
    mw_grid,
    varname = names(layers),
    startDate = paste0(yr, "-1-1"),
    endDate = paste0(yr, "-12-31"),
    verbose = T
  )

  message("resampling...")
  temps <- lapply(layers, function(nm) {
    grid <- gridMET[[nm]] - 273.15
    extr <- exact_extract(grid, mw_grid, "mean")
    mw_grid %>%
      st_drop_geometry() %>%
      bind_cols(extr) %>%
      pivot_longer(
        starts_with("mean"),
        names_to = "date",
        names_transform = ~str_split_i(.x, "_", 2),
        values_to = nm
      )
  })

  df <- left_join(temps[[1]], temps[[2]])

  message("saving...")
  saveRDS(df, fname)
  fname
})

# test
readRDS("data/1994.rds") %>%
  as_tibble() %>%
  rename(min_temp = daily_minimum_temperature, max_temp = daily_maximum_temperature)



# Process weather in parallel ---------------------------------------------

# load saved temperature data for a year
build_weather <- function(yr) {
  infile <- paste0("data/", yr, ".rds")
  outfile <- paste0("data/weather_", yr, ".rds")

  if (file.exists(outfile)) {
    message("Output file ", outfile, " already exists, skipping...")
    return()
  }

  message("loading ", fname)
  df <- readRDS(fname)

  # rename and set dates
  cat("processing...")
  df <- df %>%
    rename(
      min_temp = daily_minimum_temperature,
      max_temp = daily_maximum_temperature
    ) %>%
    mutate(
      date = as_date(date),
      year = year(date),
      yday = yday(date),
      .after = date
    )

  # check temps
  cat("checking...")
  df <- bind_rows(
    df %>% filter(min_temp <= max_temp),
    df %>%
      filter(min_temp > max_temp) %>%
      rename(min_temp = max_temp, max_temp = min_temp)
  ) %>%
    arrange(date, lat, lng)

  # convert temps, add frost, calc gdd
  cat("gdd...")
  df <- df %>%
    mutate(
      # convert temps to F
      min_temp = min_temp * 1.8 + 32,
      max_temp = max_temp * 1.8 + 32,

      # create frost flags
      frost = min_temp <= 32,
      freeze = min_temp <= 28,
      kill = min_temp <= 24,

      # calc gdd
      gdd41 = gdd_sine(min_temp, max_temp, 41, 86),
      gdd50 = gdd_sine(min_temp, max_temp, 50, 86),

      # round to 8
      across(starts_with("gdd"), ~round(.x, 8))
    )

  # save data
  cat("saving...")
  saveRDS(df, outfile)
  cat(outfile)

  df
}

# wx <- build_weather(1994)

# build weather for each year
foreach(yr = 1995:2024, .packages = "tidyverse") %dopar% {
  build_weather(yr)
}



# Generate climate statistics --------------------------------------------------

summarize_climate <- function(df) {
  df %>%
    summarize(
      across(c(min_temp, max_temp, gdd41, gdd50), ~mean(.x, na.rm = T)),
      across(c(frost, freeze, kill), ~ sum(.x, na.rm = T) / n()),
      .by = c(lat, lng, yday)
    )
}

# add cumulative prob of frost after date / no frost til date

add_cumul_probs <- function(df) {
  spring <- df %>%
    filter(yday < 200) %>%
    arrange(lat, lng, desc(yday)) %>%
    mutate(
      frost_by = 1 - cumprod(1 - frost),
      freeze_by = 1 - cumprod(1 - freeze),
      kill_by = 1 - cumprod(1 - kill),
      .by = c(lat, lng)
    )

  fall <- df %>%
    filter(yday >= 200) %>%
    arrange(lat, lng, yday) %>%
    mutate(
      frost_by = 1 - cumprod(1 - frost),
      freeze_by = 1 - cumprod(1 - freeze),
      kill_by = 1 - cumprod(1 - kill),
      .by = c(lat, lng)
    )

  bind_rows(spring, fall) %>%
    arrange(lat, lng, yday)
}


## Load weather data ----
# takes a minute

wx_full <- list.files(path = "data", pattern = "weather_*", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows()


## Process results ----

climates <- list(
  "30yr" = OPTS$yrs_30,
  "10yr" = OPTS$yrs_10,
  "5yr" = OPTS$yrs_5
)

# create climate datasets
imap(climates, function(value, name) {
  min_yr <- min(value)
  max_yr <- max(value)

  message("Building ", name, " climate ==> ", min_yr, "-", max_yr)

  fname <- paste("climate", name, min_yr, max_yr, sep = "_")
  rds_file <- sprintf("data/%s.rds", fname)
  fst_file <- sprintf("data/%s.fst", fname)

  if (file.exists(rds_file) & file.exists(fst_file)) {
    cat("output files already exist, skipping...")
    return("skipped")
  }

  # split by lat for parallel processing
  cat("preparing...")
  wx <- wx_full %>%
    filter(year >= min_yr) %>%
    split(.$lat)

  # summarize
  cat("summarizing...")
  climate <-
    foreach(
      df = wx,
      .packages = "tidyverse",
      .combine = bind_rows,
      .export = c("summarize_climate", "add_cumul_probs")
    ) %dopar% {
      df %>%
        summarize_climate() %>%
        add_cumul_probs()
    }

  # export
  cat("saving...")
  climate %>% saveRDS(rds_file)
  climate %>% write_fst(fst_file, compress = 99)
  "complete"
})



# Inspect climate data ---------------------------------------------------------

## load data ----

climate <- list(
  "5yr" = "data/climate_5yr_2019_2024.fst",
  "10yr" = "data/climate_10yr_2014_2024.fst",
  "30yr" = "data/climate_30yr_1994_2024.fst"
) %>%
  lapply(read_fst) %>%
  lapply(as_tibble) %>%
  bind_rows(.id = "type") %>%
  mutate(across(type, fct_inorder))


## build grid ----

climate_grid <- climate %>%
  distinct(lat, lng) %>%
  mutate(geometry = st_sfc(points_to_grid(lat, lng), crs = 4326), .by = c(lat, lng)) %>%
  st_set_geometry("geometry")

# should show correct grid which omits canada and great lakes
climate_grid %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(weight = .25, opacity = .5, fillOpacity = 0)

# should show same
climate %>%
  distinct(lat, lng) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRectangles(
    lat1 = ~lat - .05, lat2 = ~lat + .05,
    lng1 = ~lng - .05, lng2 = ~lng + .05,
    weight = .25,
    opacity = .5,
    fillOpacity = 0
  )


# inspect climate data ----

climate %>%
  filter(type == "30yr") %>%
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

climate %>%
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) %>%
  ggplot(aes(x = yday, y = gdd41, color = type)) +
  geom_line() +
  geom_smooth(method = "gam") +
  labs(title = "Mean GDD41 per day")

climate %>%
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) %>%
  ggplot(aes(x = yday, y = freeze, color = type)) +
  geom_line() +
  geom_smooth(method = "gam")

climate %>%
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) %>%
  mutate(date = as_date("2024-1-1") + yday - 1) %>%
  plot_ly(x = ~date, color = ~type) %>%
  add_lines(y = ~frost_by, name = ~paste(type, "prob of frost by date")) %>%
  add_lines(y = ~freeze_by, name = ~paste(type, "prob of freezing by date"))



# County map ----

us_counties <- read_sf("cb-2018-conus-county-5m.geojson")
us_states <- read_sf("cb-2018-conus-state-5m.geojson")

climate_grids <- climate_10yr %>%
  distinct(lat, lng) %>%
  mutate(keep = T) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

keep_counties <- climate_grids %>%
  st_join(us_counties) %>%
  drop_na(NAME) %>%
  distinct(STATEFP, COUNTYFP)

select_counties <- us_counties %>%
  inner_join(keep_counties) %>%
  st_join(us_states %>% select(state = NAME, geometry), largest = T) %>%
  select(state, county = NAME, geometry) %>%
  arrange(state, county)

select_counties %>% saveRDS("mw-counties.rds")

select_counties %>%
  leaflet() %>%
  addPolygons(
    label = ~paste0(state, ": ", county, " County"),
    fillColor = ~colorFactor(grDevices::rainbow(n_distinct(state)), state)(state),
    weight = 1
  )

wi_counties <- read_rds("counties-wi.rds") %>%
  select(county = CountyName, dnr_region = DnrRegion, geometry)

wi_counties %>%
  saveRDS("counties-wi.rds")
