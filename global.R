#- global.R -#

suppressMessages({
  # library(rlang) # walrus operator
  library(tidyverse) # core
  library(sf) # spatial
  library(fst) # file storage
  library(shiny)
  library(shinyBS) # bscollapse
  library(shinyjs) # javascript
  library(shinythemes) # theme
  library(shinyWidgets) # radioGroupButtons
  library(htmltools) # tagList
  library(shinycssloaders) # withSpinner
  # library(DT) # data tables
  library(leaflet) # map
  library(leaflet.extras) # map JS buttons
  library(plotly) # plots
  library(markdown) #includeMarkdown
  library(RColorBrewer)
})

# allow bundle size > 1gb
options(rsconnect.max.bundle.size = 5e9)


# Utility functions ----

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", typeof(x), ">")
  print(x)
}

# swap names and values in a list
invert <- function(x) {
  y <- as(names(x), class(x))
  names(y) <- x
  y
}

# return the first truthy argument
first_truthy <- function(...) {
  for (arg in list(...)) if (shiny::isTruthy(arg)) return(arg)
  NULL
}

# for agweather
yesterday <- function() {
  as.Date(format(Sys.time() - 60 * 60, tz = "America/Chicago")) - 1
}

make_date <- function(y, m, d) {
  as_date(paste(y, m, d, sep = "-"))
}

# accepts date or year
start_of_year <- function(d = Sys.Date()) {
  if (is.Date(d)) d <- year(d)
  make_date(d, 1, 1)
}

# accepts date or year
end_of_year <- function(d = Sys.Date()) {
  if (is.Date(d)) d <- year(d)
  make_date(d, 12, 31)
}

align_dates <- function(target_date, ref_date) {
  start_of_year(ref_date) + yday(target_date) - 1
}

clamp <- function(x, left, right) {
  if (is.null(x)) return()
  min(max(left, x), right)
}

#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @returns single sine growing degree days for one day
gdd_sine <- function(tmin, tmax, base) {
  mapply(function(tmin, tmax, base) {
    if (is.na(tmin) || is.na(tmax)) return(NA)

    # swap min and max if in wrong order for some reason
    if (tmin > tmax) { t = tmin; tmin = tmax; tmax = t }

    # min and max < lower
    if (tmax <= base) return(0)

    average = (tmin + tmax) / 2

    # tmin > lower = simple average gdds
    if (tmin >= base) return(average - base)

    # tmin < lower, tmax > lower = sine gdds
    alpha = (tmax - tmin) / 2
    base_radians = asin((base - average) / alpha)
    a = average - base
    b = pi / 2 - base_radians
    c = alpha * cos(base_radians)
    (1 / pi) * (a * b + c)
  }, tmin, tmax, base)
}


# Defs ----

google_key <- Sys.getenv("google_places_key")
# style <- read_file("www/style.css") %>% str_replace_all("[\r\n]", " ")
cur_yr <- year(yesterday())
min_yr <- 2023
OPTS <- lst(
  # leaflet color palette
  factor_colors = {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  },

  # zoom 7 for wi, 6 for mw
  map_center = list(
    wi = c(44.8, -90),
    mw = c(44, -90)
  ),

  # map extents
  map_extent = list(
    wi = list(
      lat = c(42.4, 47.1),
      lng = c(-93.0, -86.8)
    ),
    mw = list(
      lat = c(38, 49.4),
      lng = c(-98, -82)
    )
  ),
  map_extent_choices = list(
    "Upper Midwest" = "mw",
    "Wisconsin" = "wi"
  ),
  basemaps = list(
    "ESRI Topo" = providers$Esri.WorldTopoMap,
    "Satellite" = providers$Esri.WorldImagery,
    "OpenStreetMap" = providers$OpenStreetMap,
    "Grey Canvas" = providers$CartoDB.Positron
  ),
  map_layers = list(
    grid = "Data grid",
    counties = "Counties/Regions"
  ),

  # interface settings
  debounce_ms = 200,
  weather_years = cur_yr:min_yr,
  weather_date_fmt = "%b %-d",
  climate_date_fmt = "%b %-d",
  weather_date_min = start_of_year(min_yr),
  weather_date_max = NULL, # set in server
  climate_date_min = NULL, # set in server
  climate_date_max = NULL, # set in server
  max_cut_dates = 5,
  cut_freq_choices = seq(800, 1200, by = 100),
  cut_freq_default = 1000,
  map_type_choices = list(
    "Observed weather" = "weather",
    "Climate averages" = "climate",
    "Weather vs climate" = "comparison"
  ),
  climate_period_choices = list(
    "10-year climate average (2014-2024)" = "c10",
    "5-year climate average (2019-2025)" = "c5"
  ),
  climate_frost_choices = list(
    "Frost (<32°F)" = "frost",
    "Hard freeze (<28°F)" = "freeze",
    "Killing freeze (<24°F)" = "kill"
  ),
  data_smoothing_choices = list(
    "No smoothing" = "1",
    "7-day average" = "7",
    "14-day average" = "14",
    "28-day average" = "28"
  ),
  custom_plot_elems = list(
    "Weather - Temperature" = "weather_temp",
    "Weather - GDD/day" = "weather_gdd",
    "Weather - Cumulative GDD" = "weather_gddcum",
    "Climate - Temperature" = "climate_temp",
    "Climate - GDD/day" = "climate_gdd",
    "Climate - Cumulative GDD" = "climate_gddcum",
    "Climate - Frost/freeze/kill prob." = "climate_frost"
  ),

  # boilerplate
  load_error_msg = "Warning: Weather could not be loaded for all dates. Please contact the developer if you encounter any issues.",
  location_validation_msg = "Please select a location on the map first. Use the crosshair icon on the map to automatically select your location, or enter a place name in the searchbox below the map.",
  weather_plot_caption = "Today's date is indicated as a vertical dashed line. Click on any item in the plot legend to toggle it on or off. Click and drag on the plot to zoom in, double click to reset view. Click on the camera icon in the plot menu to download a copy of the plot.",

  # plotly settings
  plot_date_axis_climate = list(
    title = "Date",
    dtick = "M1",
    tickformat = "%b",
    hoverformat = "%b %d (day %j)",
    domain = c(0, .95)),
  plot_date_axis_weather = list(
    title = "Date",
    dtick = "M1",
    tickformat = "%b",
    hoverformat = "%b %d, %Y (day %j)",
    domain = c(0, .95)),
  plot_legend = list(
    orientation = "h",
    xanchor = "center",
    x = .5, y = -.15),
  plot_line_width = 1.5,
  plot_colors = list(
    min_temp = "cornflowerblue",
    mean_temp = "orange",
    max_temp = "red",
    frost = "orchid",
    freeze = "purple",
    kill = "darkslateblue",
    gdd41 = "green",
    gdd50 = "peru",
    gdd41cum = "olivedrab",
    gdd50cum = "chocolate"
  ),

  # column defs
  cumulative_cols = c("gdd41cum", "gdd50cum"),
  percent_cols = c("frost", "freeze", "kill", "frost_by", "freeze_by", "kill_by"),
  comparison_cols = c("min_temp", "max_temp", "mean_temp", "gdd41", "gdd50"),
  smoothable_cols = c("min_temp", "max_temp", "mean_temp", "gdd41", "gdd50", "frost", "freeze", "kill", "frost_by", "freeze_by", "kill_by"),
  grid_cols = list(
    weather = list(
      "Mean temperature (°F)" = "mean_temp",
      "Min temperature (°F)" = "min_temp",
      "Max temperature (°F)" = "max_temp",
      "Daily GDD41 accumulation" = "gdd41",
      "Daily GDD50 accumulation" = "gdd50",
      "Cumulative GDD41" = "gdd41cum",
      "Cumulative GDD50" = "gdd50cum"
    ),
    climate = list(
      "Mean temperature (°F)" = "mean_temp",
      "Min temperature (°F)" = "min_temp",
      "Max temperature (°F)" = "max_temp",
      "Mean daily GDD41" = "gdd41",
      "Mean daily GDD50" = "gdd50",
      "Mean cumul. GDD41" = "gdd41cum",
      "Mean cumul. GDD50" = "gdd50cum",
      "Prob. of frost (<32°F) on day" = "frost",
      "Prob. of hard freeze (<28°F) on day" = "freeze",
      "Prob. of killing freeze (<24°F) on day" = "kill",
      "Cumul. prob. of frost" = "frost_by",
      "Cumul. prob. of hard freeze" = "freeze_by",
      "Cumul. prob. of killing freeze" = "kill_by"
    ),
    comparison = list(
      "Mean daily temp vs climate average (°F)" = "mean_temp",
      "Min daily temp vs climate average (°F)" = "min_temp",
      "Max daily temp vs climate average (°F)" = "max_temp",
      "Daily GDD41 vs climate average" = "gdd41",
      "Daily GDD50 vs climate average" = "gdd50",
      "Cumul. GDD41 vs climate average" = "gdd41cum",
      "Cumul. GDD50 vs climate average" = "gdd50cum"
    )
  )
)


## UI builders ----

withSpinnerProxy <- function(ui, ...) {
  ui %>% shinycssloaders::withSpinner(type = 8, color = "#30a67d", proxy.height = "400px", ...)
}


## Helper functions ----

# returns the label associated with a grid type and value
get_col_label <- function(type, col) {
  invert(OPTS$grid_cols[[type]])[[col]]
}

# converts the incoming json coordinates in the form '[lat, lng]' to cols
fix_coords <- function(df) {
  df %>%
    mutate(
      name = gsub("\\[|\\]|\\s", "", name),
      lat = as.numeric(str_split_i(name, ",", 1)),
      lng = as.numeric(str_split_i(name, ",", 2)),
      .after = name
    ) %>%
    select(-name)
}

coords_to_pt <- function(lat, lng) {
  sprintf("%.1f %.1f", lat, lng)
}

pt_to_coords <- function(pt) {
  list(
    lat = as.numeric(str_split_i(pt, " ", 1)),
    lng = as.numeric(str_split_i(pt, " ", 2))
  )
}

# extent may be 'wi' or 'mw'
in_extent <- function(lat, lng, extent) {
  extent <- OPTS$map_extent[[extent]]
  between(lat, extent$lat[1], extent$lat[2]) &
    between(lng, extent$lng[1], extent$lng[2])
}

# potentially supports multiple extents, but for now just 'wi' or everything
filter_by_extent <- function(.data, extent) {
  if (extent == "wi") {
    .data %>% filter(in_extent(lat, lng, extent = "wi"))
  } else {
    .data
  }
}

parse_coords <- function(str) {
  str <- gsub("[ °NW]", "", str)
  parts <- str_split_1(str, ",")
  if (length(parts) != 2) stop("Invalid coordinate format.")
  coords <- suppressWarnings(list(
    lat = as.numeric(parts[1]),
    lng = as.numeric(parts[2])
  ))
  if (any(sapply(coords, is.na))) stop("Failed to parse coordinates.")
  coords
}

add_climate_cols <- function(.data) {
  .data %>%
    mutate(
      mean_temp = rowMeans(pick(min_temp, max_temp)),
      .after = max_temp
    ) %>%
    mutate(
      gdd41cum = cumsum(gdd41),
      gdd50cum = cumsum(gdd50),
      .by = c(lat, lng),
      .after = gdd50
    )
}

smooth_cols <- function(.data, width, cols = OPTS$smoothable_cols) {
  width <- as.numeric(width)
  if (width == 1) return(.data)
  mutate(.data, across(
    any_of(cols),
    ~zoo::rollapply(.x, width = width, FUN = mean, na.rm = T, partial = T)
  ))
}

remove_weather_cols <- function(.data) {
  drop_cols <- c("year", "yday", "mean_temp", "frost", "freeze", "kill", "gdd41cum", "gdd50cum", "inwi")
  .data %>% select(-any_of(drop_cols))
}

add_weather_cols <- function(.data) {
  .data %>%
    arrange(lat, lng, date) %>%
    mutate(
      year = as.integer(year(date)),
      yday = as.integer(yday(date)),
      mean_temp = rowMeans(pick(min_temp, max_temp)),
      frost = min_temp <= 32,
      freeze = min_temp <= 28,
      kill = min_temp <= 24
    ) %>%
    mutate(
      gdd41cum = cumsum(gdd41),
      gdd50cum = cumsum(gdd50),
      .by = c(lat, lng, year)
    ) %>%
    select(
      lat, lng, date, year, yday,
      min_temp, max_temp, mean_temp,
      gdd41, gdd50, gdd41cum, gdd50cum,
      frost, freeze, kill
    )
}

# units: temp=F, pressure=kPa, rh=%
get_weather_grid <- function(d = yesterday()) {
  url <- paste0("https://agweather.cals.wisc.edu/api/weather/grid?date=", d)
  message(d, " ==> GET ", url)
  wx <- tibble()
  tryCatch({
    resp <- httr::GET(url) %>% httr::content()
    data <- resp$data %>%
      enframe() %>%
      unnest_wider("value")
    if (nrow(data) == 0) stop()
    wx <- data %>%
      fix_coords() %>%
      select(lat, lng, date, min_temp, max_temp) %>%
      inner_join(climate_grids, join_by(lat, lng)) %>%
      mutate(
        date = as_date(d),
        gdd86 = gdd_sine(min_temp, max_temp, 86),
        gdd41 = round(gdd_sine(min_temp, max_temp, 41) - gdd86, 8),
        gdd50 = round(gdd_sine(min_temp, max_temp, 50) - gdd86, 8)
      ) %>%
      select(-gdd86)
  }, error = function(e) {
    message(str_glue("Failed to retrieve weather data for {d}: {e}"))
  })
  wx
}

weather_dates <- function() {
  dates_need <- sort(seq.Date(OPTS$weather_date_min, yesterday(), 1))
  dates_have <- if (exists("weather")) sort(unique(weather$date))
  as.character(dates_need[!(dates_need %in% dates_have)])
}


# Data load functions ----


load_climate <- function() {
  if (!exists("climate")) {
    climate <<- list(
      c5 = read_fst("data/climate_5yr.fst"),
      c10 = read_fst("data/climate_10yr.fst")
    ) %>%
      lapply(as_tibble) %>%
      lapply(add_climate_cols)
  }
  if(!exists("climate_grids")) {
    climate_grids <<- climate$c10 %>% distinct(lat, lng)
  }
}

load_weather <- function() {
  wx_files <- list.files(path = "./data", pattern = "^weather_.*\\.fst$", full.names = T)
  if (length(wx_files) > 0) {
    if (!exists("weather") || max(weather$date) != yesterday()) {
      weather <<- wx_files %>%
        lapply(read_fst) %>%
        bind_rows() %>%
        as_tibble()
    }
  }
}

update_weather <- function(dates = weather_dates(), progress = FALSE) {
  if (!exists("weather")) weather <<- tibble()

  # fetch new weather
  if (length(dates) > 0) {
    new_weather <- lapply(dates, function(d) {
      if (progress) incProgress(1 / length(dates), message = "Updating weather...", detail = paste("Fetching", format(as_date(d), "%b %d, %Y")))
      get_weather_grid(d)
    }) %>% bind_rows()
    weather <<- bind_rows(weather, new_weather)
  }

  if (progress) incProgress(1, message = "Finalizing datasets...", detail = "")
  weather <<- weather %>% add_weather_cols()

  # save weather file(s) if updated
  lapply(unique(year(as_date(dates))), function(yr) {
    weather %>%
      filter(year == yr) %>%
      remove_weather_cols() %>%
      write_fst(str_glue("data/weather_{yr}.fst"), compress = 99)
  })
}

load_data <- function() {
  withProgress(
    message = "Loading datasets...",
    value = 0, min = 0, max = 3,
    {
      load_climate()
      load_weather()
      dates <- weather_dates()
      incProgress(ifelse(length(dates) > 0, 1, 2))
      update_weather(dates, progress = T)
    }
  )
}


# Initialize data ----

list.files("R", "*.R", full.names = T) %>% sapply(source)

if (!exists("counties_wi")) {
  counties_wi <- read_rds("data/counties_wi.rds") %>%
    mutate(
      label = paste0("<b>", county, " County</b><br>", dnr_region) %>%
        lapply(shiny::HTML)
    )
}

if (!exists("counties_mw")) {
  counties_mw <- read_rds("data/counties_mw.rds") %>%
    mutate(
      label = paste0("<b>", state, "</b><br>", county, " County") %>%
        lapply(shiny::HTML)
    )
}



# App cleanup ----

# removes all objects from the environment on app shutdown
# onStop(
#   function() { rm(list = ls(all.names = TRUE)) }
# )


# Testing ----

# delete some weather for testing
# weather <- weather %>% filter(date < Sys.Date() - 1)
# weather %>%
#   filter(year == 2025) %>%
#   remove_weather_cols() %>%
#   write_fst("data/weather_2025.fst", compress = 99)

# weather
#
# rbenchmark::benchmark(
#   filter = {
#     weather %>% filter(in_extent(lat, lng))
#   },
#   inwi = {
#     weather %>% filter(inwi)
#   },
#   replications = 1
# )
#
#
# rbenchmark::benchmark(
#   climate = {
#     load_climate()
#   },
#   weather = {
#     load_weather()
#   },
#   replications = 1
# )
