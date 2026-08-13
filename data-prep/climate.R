#' Generate climate data for the Alfalfa weather tool

# remotes::install_github("mikejohnson51/climateR")

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(climateR)
library(exactextractr)
library(fst)
library(leaflet)
library(plotly)
library(dtplyr)
library(data.table)
library(foreach) # Parallel processing
library(doParallel)
library(skimr) # for `skim()` data viewer

# Set up parallel backend
registerDoParallel(cores = detectCores() - 1)

# Stop parallel backend
# stopImplicitCluster()

# settings
OPTS <- lst(
  max_yr = year(Sys.Date()) - 1,
  yrs_30 = seq(max_yr - 30, max_yr, 1),
  yrs_10 = seq(max_yr - 10, max_yr, 1),
  yrs_5 = seq(max_yr - 5, max_yr, 1),
  extent_wi = list(
    lat = c(42.4, 47.1),
    lng = c(-93.0, -86.8)
  ),
  extent_mw = list(
    lat = c(38, 50),
    lng = c(-98, -82)
  )
)

#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @param upper upper temperature threshold
#' @returns single sine growing degree days for one day
gdd_sine <- function(tmin, tmax, base, upper = 150) {
  tmin_adj <- pmin(tmin, tmax)
  tmax_adj <- pmax(tmin, tmax)

  avg <- (tmin_adj + tmax_adj) / 2
  alpha <- (tmax_adj - tmin_adj) / 2
  safe_alpha <- pmax(alpha, .Machine$double.eps)

  base_rad <- asin(pmax(-1, pmin(1, (base - avg) / safe_alpha)))
  upper_rad <- asin(pmax(-1, pmin(1, (upper - avg) / safe_alpha)))

  val_simple <- avg - base
  val_sine <- (1 / pi) *
    ((avg - base) * (pi / 2 - base_rad) + alpha * cos(base_rad))
  val_both <- (1 / pi) *
    ((avg - base) *
      (upper_rad - base_rad) +
      alpha * (cos(base_rad) - cos(upper_rad)) +
      (upper - base) * (pi / 2 - upper_rad))

  dplyr::case_when(
    is.na(tmin) | is.na(tmax) ~ NA_real_,
    tmax_adj <= base ~ 0,
    tmin_adj >= upper ~ upper - base, # both thresholds exceeded
    tmin_adj >= base ~ val_simple,
    tmax_adj <= upper ~ val_sine,
    TRUE ~ val_both
  )
}

# test gdd function
if (FALSE) {
  tibble(
    tmin = runif(100),
    tmax = runif(100),
    gdd = gdd_sine(tmin, tmax, .5, .8),
    cumgdd = cumsum(gdd)
  ) |>
    mutate(day = row_number()) |>
    ggplot(aes(x = day)) +
    geom_col(aes(y = gdd)) +
    geom_line(aes(y = cumgdd))
}


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
  lats <- seq(extent$lat[1], extent$lat[2], .1)
  lngs <- seq(extent$lng[1], extent$lng[2], .1)
  tibble(lat = lats) |>
    reframe(lng = lngs, .by = lat) |>
    mutate(
      geometry = st_sfc(points_to_grid(lat, lng), crs = 4326),
      .by = c(lat, lng)
    ) |>
    st_set_geometry("geometry")
}

if (FALSE) {
  wi_grid <- grid_from_extent(OPTS$extent_wi)
  mw_grid <- grid_from_extent(OPTS$extent_mw)
  wi_grid |> saveRDS("data/grids/wi_grid.rds")
  wi_grid |> write_sf("data/grids/wi_grid.geojson", delete_dsn = TRUE)
  mw_grid |> saveRDS("data/grids/mw_grid.rds")
  mw_grid |> write_sf("data/grids/mw_grid.geojson", delete_dsn = TRUE)

  # should show checkerboard pattern to check for correct grid size
  wi_grid |>
    mutate(row = row_number()) |>
    filter(row %% 2 == 0) |>
    leaflet() |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(
      data = wi_boundary,
      weight = 1,
      color = "black",
      fillOpacity = 0
    ) |>
    addPolygons(weight = .1, label = ~ str_glue("{lat}, {lng}"))

  mw_grid |>
    mutate(row = row_number()) |>
    filter(row %% 2 == 0) |>
    leaflet() |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(
      data = mw_boundary,
      weight = 1,
      color = "black",
      fillOpacity = 0
    ) |>
    addPolygons(weight = .1, label = ~ str_glue("{lat}, {lng}"))
}

wi_grid <- readRDS("data/grids/wi_grid.rds")
mw_grid <- readRDS("data/grids/mw_grid.rds")

wi_boundary <- wi_grid |> st_bbox() |> st_as_sfc()
mw_boundary <- mw_grid |> st_bbox() |> st_as_sfc()


# Download weather data --------------------------------------------------------

layers <- list(
  "tmmn" = "daily_minimum_temperature",
  "tmmx" = "daily_maximum_temperature"
)

lapply(OPTS$yrs_30, function(yr) {
  fname <- paste0("data/gridMET/", yr, ".rds")

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
    mw_grid |>
      st_drop_geometry() |>
      bind_cols(extr) |>
      pivot_longer(
        starts_with("mean"),
        names_to = "date",
        names_transform = ~ str_split_i(.x, "_", 2),
        values_to = nm
      )
  })

  df <- left_join(temps[[1]], temps[[2]])

  message("saving...")
  saveRDS(df, fname)
  fname
})

# test
if (FALSE) {
  readRDS("data/gridMET/2025.rds") |>
    as_tibble() |>
    rename(
      min_temp = daily_minimum_temperature,
      max_temp = daily_maximum_temperature
    )
}


# Process weather in parallel ---------------------------------------------

# load saved temperature data for a year
build_weather <- function(yr, force = FALSE) {
  infile <- paste0("data/gridMET/", yr, ".rds")
  outfile <- paste0("data/weather/weather_", yr, ".rds")

  if (file.exists(outfile) && isFALSE(force)) {
    message("Output file ", outfile, " already exists, skipping...")
    return(NULL)
  }

  message("Loading ", infile)
  df <- readRDS(infile)

  message("Processing...")

  # Ensure data.table doesn't fight foreach for CPU threads
  setDTthreads(1)

  df <- df |>
    lazy_dt() |>
    rename(
      min_temp = daily_minimum_temperature,
      max_temp = daily_maximum_temperature
    ) |>
    # Drop nan values from gridMET over great lakes
    filter(!is.nan(min_temp), !is.nan(max_temp)) |>
    mutate(
      date = as_date(date),
      year = year(date),
      yday = yday(date),

      # 1. Fix swapped temps inline (much faster than filter/bind_rows)
      actual_min = if_else(min_temp > max_temp, max_temp, min_temp),
      actual_max = if_else(min_temp > max_temp, min_temp, max_temp),

      # 2. Convert temps to F directly from the corrected values
      min_temp = actual_min * 1.8 + 32,
      max_temp = actual_max * 1.8 + 32,

      # 3. Create frost flags
      frost = min_temp <= 32,
      freeze = min_temp <= 28,
      kill = min_temp <= 24,

      # 4. Calc GDD and round inline
      gdd41 = round(gdd_sine(min_temp, max_temp, 41, 86), 8),
      gdd50 = round(gdd_sine(min_temp, max_temp, 50, 86), 8)
    ) |>
    # Drop intermediate columns
    select(-actual_min, -actual_max) |>
    arrange(date, lat, lng) |>
    as_tibble()

  message("Saving to ", outfile)
  saveRDS(df, outfile)
  gc()

  return(df)
}

if (FALSE) {
  build_weather(1995, TRUE)
}

# build weather for each year
# (Assuming your parallel backend like doParallel is registered before this)
foreach(
  yr = OPTS$yrs_30,
  .packages = c("tidyverse", "dtplyr", "data.table")
) %dopar%
  {
    build_weather(yr)
  }

# Generate climate statistics --------------------------------------------------

summarize_climate <- function(df) {
  df |>
    lazy_dt() |>
    group_by(lat, lng, yday) |>
    summarize(
      across(c(min_temp, max_temp, gdd41, gdd50), ~ mean(.x, na.rm = TRUE)),
      across(c(frost, freeze, kill), ~ sum(.x, na.rm = TRUE) / n())
    ) |>
    ungroup() |>
    as_tibble()
}

add_cumul_probs <- function(df) {
  d <- 200

  spring <- df |>
    filter(yday < d) |>
    arrange(lat, lng, desc(yday)) |>
    mutate(
      frost_by = 1 - cumprod(1 - frost),
      freeze_by = 1 - cumprod(1 - freeze),
      kill_by = 1 - cumprod(1 - kill),
      .by = c(lat, lng)
    )

  fall <- df |>
    filter(yday >= d) |>
    arrange(lat, lng, yday) |>
    mutate(
      frost_by = 1 - cumprod(1 - frost),
      freeze_by = 1 - cumprod(1 - freeze),
      kill_by = 1 - cumprod(1 - kill),
      .by = c(lat, lng)
    )

  bind_rows(spring, fall) |>
    arrange(lat, lng, yday)
}


if (FALSE) {
  wx <- sprintf("data/weather/weather_%s.rds", OPTS$yrs_5) |>
    lapply(readRDS) |>
    bind_rows()
  wx |>
    distinct(lat, lng) |>
    mutate(
      geometry = st_sfc(points_to_grid(lat, lng), crs = 4326),
      .by = c(lat, lng)
    ) |>
    st_set_geometry("geometry") |>
    leaflet() |>
    addPolygons()

  clim <- wx |>
    filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
    summarize_climate()
  clim |>
    ggplot(aes(yday, frost)) +
    geom_line()
  clim2 <- add_cumul_probs(clim)
  clim2 |>
    filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
    ggplot(aes(x = yday, y = frost_by)) +
    geom_line()
}


## Load weather data ----
# takes a minute

wx_full <- sprintf("data/weather/weather_%s.rds", OPTS$yrs_30) |>
  lapply(readRDS) |>
  bind_rows()

if (FALSE) {
  climate_30yr <- process_climate_data(wx_full)
}


## Process results ----

climates <- list(
  "30yr" = OPTS$yrs_30,
  "10yr" = OPTS$yrs_10,
  "5yr" = OPTS$yrs_5
)

# create or read climate datasets
climate <- imap(climates, function(value, name, force = TRUE) {
  min_yr <- min(value)
  max_yr <- max(value)

  message("Building ", name, " climate ==> ", min_yr, "-", max_yr)

  fname <- paste("climate", name, min_yr, max_yr, sep = "_")
  fst_file <- sprintf("data/climate/%s.fst", fname)

  if (file.exists(fst_file) & isFALSE(force)) {
    message("Output files already exist, skipping...")
    return(as_tibble(read_fst(fst_file)))
  }

  message("Filtering and Summarizing...")
  setDTthreads(0) # unlimited since we're not parallel
  climate <- wx_full |>
    filter(year >= min_yr & year <= max_yr) |>
    summarize_climate() |>
    add_cumul_probs()

  message("Saving...")
  write_fst(climate, fst_file, compress = 99)
  message("Complete")
  climate
}) |>
  bind_rows(.id = "type") |>
  mutate(across(type, fct_inorder))


# Inspect climate data ---------------------------------------------------------

## load data ----

skim(climate)

# should show no nan values
climate |>
  summarize(
    n_nan = sum(is.nan(min_temp)),
    .by = c(lat, lng)
  ) |>
  arrange(desc(n_nan))


## build grid ----

climate_grid <- climate |>
  distinct(lat, lng) |>
  mutate(
    geometry = st_sfc(points_to_grid(lat, lng), crs = 4326),
    .by = c(lat, lng)
  ) |>
  st_set_geometry("geometry")

# should show correct grid which omits canada and great lakes
climate_grid |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(weight = 0.25, opacity = 0.5, fillOpacity = 0)

# should show same
climate |>
  distinct(lat, lng) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addRectangles(
    lat1 = ~ lat - 0.05,
    lat2 = ~ lat + 0.05,
    lng1 = ~ lng - 0.05,
    lng2 = ~ lng + 0.05,
    weight = 0.25,
    opacity = 0.5,
    fillOpacity = 0
  )


# Inspect climate data ----

climate |>
  filter(type == "30yr") |>
  filter(yday == 150) |>
  rename(value = max_temp) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addRectangles(
    lat1 = ~ lat - .05,
    lat2 = ~ lat + .05,
    lng1 = ~ lng - .05,
    lng2 = ~ lng + .05,
    weight = .5,
    opacity = .1,
    color = "black",
    fillOpacity = .75,
    fillColor = ~ colorNumeric("viridis", value)(value),
    label = ~ round(value, 1)
  )

climate |>
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
  ggplot(aes(x = yday, y = gdd41, color = type)) +
  geom_line() +
  geom_smooth(method = "gam") +
  labs(title = "Mean GDD41 per day")

climate |>
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
  ggplot(aes(x = yday, y = freeze, color = type)) +
  geom_line() +
  geom_smooth(method = "gam")

climate |>
  filter(lat == sample(lat, 1), lng == sample(lng, 1)) |>
  mutate(date = as_date("2024-1-1") + yday - 1) |>
  plot_ly(x = ~date, color = ~type) |>
  add_lines(y = ~frost_by, name = ~ paste(type, "prob of frost by date")) |>
  add_lines(y = ~freeze_by, name = ~ paste(type, "prob of freezing by date"))


# County map ----

us_counties <- read_sf("cb-2018-conus-county-5m.geojson")
us_states <- read_sf("cb-2018-conus-state-5m.geojson")

climate_grids <- climate_10yr |>
  distinct(lat, lng) |>
  mutate(keep = T) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

keep_counties <- climate_grids |>
  st_join(us_counties) |>
  drop_na(NAME) |>
  distinct(STATEFP, COUNTYFP)

select_counties <- us_counties |>
  inner_join(keep_counties) |>
  st_join(us_states |> select(state = NAME, geometry), largest = T) |>
  select(state, county = NAME, geometry) |>
  arrange(state, county)

select_counties |> saveRDS("mw-counties.rds")

select_counties |>
  leaflet() |>
  addPolygons(
    label = ~ paste0(state, ": ", county, " County"),
    fillColor = ~ colorFactor(grDevices::rainbow(n_distinct(state)), state)(
      state
    ),
    weight = 1
  )

wi_counties <- read_rds("counties-wi.rds") |>
  select(county = CountyName, dnr_region = DnrRegion, geometry)

wi_counties |>
  saveRDS("counties-wi.rds")
