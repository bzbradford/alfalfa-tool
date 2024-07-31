#- global.R -#

suppressMessages({
  # library(rlang) # walrus operator
  library(tidyverse) # core
  library(sf) # spatial
  library(feather) # file storage
  library(shiny)
  library(shinyBS) # bscollapse
  library(shinyjs) # javascript
  library(shinythemes) # theme
  # library(shinyWidgets) # radioGroupButtons
  library(htmltools) # tagList
  library(shinycssloaders) # withSpinner
  # library(DT) # data tables
  library(leaflet) # map
  library(leaflet.extras) # map JS buttons
  library(plotly) # plots
})



# Utility functions ----

lapply2 <- function(x, fun) {
  setNames(
    lapply(x, fun),
    x
  )
}

yesterday <- function() Sys.Date() - 1

make_date <- function(y, m, d) {
  as_date(paste(y, m, d, sep = "-"))
}

# accepts date or year
start_of_year <- function(d = Sys.Date()) {
  if (is.Date(d)) {
    make_date(year(d), 1, 1)
  } else {
    make_date(d, 1, 1)
  }
}

# accepts date or year
end_of_year <- function(d = Sys.Date()) {
  if (is.Date(d)) {
    make_date(year(d), 12, 31)
  } else {
    make_date(d, 12, 31)
  }
}

align_dates <- function(target_date, ref_date) {
  start_of_year(ref_date) + yday(target_date) - 1
}

clamp <- function(x, left, right) {
  if (is.null(x)) return()
  min(max(left, x), right)
}

calc_gdd <- function(tmin, tmax, base, upper) {
  mapply(gdd_sine, tmin, tmax, base, upper)
}

gdd_sine <- function(tmin, tmax, base, upper) {
  if (is.na(tmin) || is.na(tmax)) return(NA)

  # swap min and max if in wrong order for some reason
  if (tmin > tmax) {
    t = tmin
    tmin = tmax
    tmax = t
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
}


# Defs ----

cur_yr <- year(yesterday())
OPTS <- list(
  # map extents
  min_lat = 42.4,
  max_lat = 47.1,
  min_lng = -93.0,
  max_lng = -86.8,

  # date settings
  start_date = as_date(paste0(cur_yr - 1, "-1-1")),

  # interface settings
  weather_years = c(cur_yr, cur_yr - 1),
  weather_date_fmt = "%b %-d, %Y",
  climate_date_fmt = "%b %-d",
  weather_date_max = NULL, # set in server
  climate_date_min = NULL, # set in server
  climate_date_max = NULL, # set in server
  climate_period_choices = list(
    "10-year climate average (2013-2023)" = "c10",
    "5-year climate average (2018-2023)" = "c5"),
  data_smoothing_choices = list(
    "Daily values (no smoothing)" = 1,
    "Weekly rolling mean" = 7,
    "14-day rolling mean" = 14),
  custom_plot_elems = list(
    "Weather - Temperature" = "weather_temp",
    "Weather - GDD/day" = "weather_gdd",
    "Weather - Cumulative GDD" = "weather_gddcum",
    "Climate - Temperature" = "climate_temp",
    "Climate - GDD/day" = "climate_gdd",
    "Climate - Cumulative GDD" = "climate_gddcum",
    "Climate - Frost/Freeze probability" = "climate_frost"),

  # boilerplate
  location_validation_msg = "Please select a grid cell in the map above to view detailed weather data for that location. Use the crosshair icon on the map to automatically select your location.",

  # plotly settings
  plot_date_axis_climate = list(
    title = "Date",
    dtick = "M1", tickformat = "%b",
    hoverformat = "%b %d (day %j)",
    domain = c(0, .95)),
  plot_date_axis_weather = list(
    title = "Date",
    dtick = "M1", tickformat = "%b",
    hoverformat = "%b %d, %Y (day %j)",
    domain = c(0, .95)),
  plot_legend = list(
    orientation = "h",
    xanchor = "center",
    x = .5, y = -.15),
  plot_line_width = 1.5,

  # column defs
  cumulative_cols = c("gdd41cum", "gdd50cum"),
  percent_cols = c("frost", "freeze", "frost_by", "freeze_by"),
  comparison_cols = c("min_temp", "max_temp", "gdd41", "gdd50"),
  smoothable_weather = c("min_temp", "max_temp", "mean_temp", "gdd41", "gdd50"),
  smoothable_climate = c("min_temp", "max_temp", "mean_temp", "gdd41", "gdd50", "frost", "freeze", "frost_by", "freeze_by"),
  grid_cols = list(
    weather = list(
      "Mean daily temp (F)" = "mean_temp",
      "Min daily temp (F)" = "min_temp",
      "Max daily temp (F)" = "max_temp",
      "Daily GDD41 accumulation" = "gdd41",
      "Daily GDD50 accumulation" = "gdd50",
      "Cumulative GDD41" = "gdd41cum",
      "Cumulative GDD50" = "gdd50cum"),
    climate = list(
      "Mean daily temp (F)" = "mean_temp",
      "Min daily temp (F)" = "min_temp",
      "Max daily temp (F)" = "max_temp",
      "Mean daily GDD41" = "gdd41",
      "Mean daily GDD50" = "gdd50",
      "Mean cumulative GDD41" = "gdd41cum",
      "Mean cumulative GDD50" = "gdd50cum",
      "Mean probability of frost on day" = "frost",
      "Mean probability of hard freeze on day" = "freeze",
      "Cumulative probability of frost" = "frost_by",
      "Cumulative probability of hard freeze" = "freeze_by"),
    comparison = list(
      "Mean daily temp vs climate average (F)" = "mean_temp",
      "Min daily temp vs climate average (F)" = "min_temp",
      "Max daily temp vs climate average (F)" = "max_temp",
      "Daily GDD41 vs climate average" = "gdd41",
      "Daily GDD50 vs climate average" = "gdd50",
      "Cumul. GDD41 vs climate average" = "gdd41cum",
      "Cumul. GDD50 vs climate average" = "gdd50cum"))
)


## Helper functions ----

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

withSpinnerProxy <- function(ui, ...) {
  ui %>% shinycssloaders::withSpinner(type = 8, color = "#30a67d", proxy.height = "400px", ...)
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

smooth_weather <- function(.data, width) {
  smooth_cols(.data, OPTS$smoothable_weather, width)
}
smooth_climate <- function(.data, width) {
  smooth_cols(.data, OPTS$smoothable_climate, width)
}
smooth_cols <- function(.data, cols, width) {
  stopifnot(is.character(cols), is.numeric(width))
  if (width == 1) return(.data)
  mutate(.data, across(
    all_of(cols),
    ~zoo::rollapply(.x, width = width, FUN = mean, na.rm = T, partial = T)
  ))
}

remove_weather_cols <- function(.data) {
  drop_cols <- c("year", "yday", "mean_temp", "frost", "freeze", "gdd41cum", "gdd50cum")
  .data %>% select(-all_of(drop_cols))
}

add_weather_cols <- function(.data) {
  .data %>%
    arrange(lat, lng, date) %>%
    mutate(
      year = year(date),
      yday = yday(date),
      mean_temp = rowMeans(pick(min_temp, max_temp)),
      frost = min_temp <= 32,
      freeze = min_temp <= 28
    ) %>%
    mutate(
      gdd41cum = cumsum(gdd41),
      gdd50cum = cumsum(gdd50),
      .by = c(lat, lng, year)
    )
}

# units: temp=F, pressure=kPa, rh=%
get_weather_grid <- function(d = yesterday()) {
  url <- paste0(
    "https://agweather.cals.wisc.edu/api/weather/grid",
    "?lat_range=", OPTS$min_lat, ",", OPTS$max_lat,
    "&long_range=", OPTS$min_lng, ",", OPTS$max_lng,
    "&date=", d
  )
  message(d, " ==> GET ", url)
  resp <- httr::GET(url) %>% httr::content()
  data <- resp$data %>%
    enframe() %>%
    unnest_wider("value")
  if (nrow(data) > 0) {
    data %>%
      fix_coords() %>%
      select(lat, lng, date, min_temp, max_temp) %>%
      mutate(
        date = as_date(d),
        gdd41 = calc_gdd(min_temp, max_temp, 41, 86),
        gdd50 = calc_gdd(min_temp, max_temp, 50, 86)
      )
  } else {
    message(str_glue("Failed to retrieve weather data for {date}"))
    tibble()
  }
}

weather_dates <- function() {
  dates_need <- sort(seq.Date(OPTS$start_date, yesterday(), 1))
  dates_have <- if (exists("weather")) sort(unique(weather$date))
  as.character(dates_need[!(dates_need %in% dates_have)])
}

# downloads and saves missing daily weather from AgWeather
fill_weather <- function(dates = weather_dates()) {
  if (length(dates) == 0) {
    message("Everything up to date. Nothing to do.")
  } else {
    for (d in dates) {
      wx <- get_weather_grid(d)
      if (!exists("weather")) {
        weather <<- wx
      } else {
        weather <<- bind_rows(weather, wx)
      }
    }
  }

  # save minimal dataset
  weather %>%
    remove_weather_cols() %>%
    write_feather("data/weather.feather")

  # build additional cols
  weather <<- weather %>% add_weather_cols()
}


# Initialize data ----

list.files("R", "*.R", full.names = T) %>% sapply(source)

counties <- read_rds("data/counties.rds")

if (!exists("climate")) {
  climate <- read_rds("data/climate.rds") %>% lapply(add_climate_cols)
}

if (file.exists("data/weather.feather")) {
  if (!exists("weather") || max(weather$date) != yesterday())
  weather <- read_feather("data/weather.feather") %>% add_weather_cols()
}

# delete some weather for testing
# weather <- weather %>% filter(date < Sys.Date() - 3)
# weather %>% write_feather("data/weather.feather")
