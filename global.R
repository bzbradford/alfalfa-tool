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


# Defs ----

MIN_LAT = 42.4
MAX_LAT = 47.1
MIN_LNG = -93.0
MAX_LNG = -86.8
START_DATE = as_date("2024-01-01")
grid_cols <- list(
  weather = list(
    "Max daily temp (F)" = "max_temp",
    "Min daily temp (F)" = "min_temp",
    "Daily GDD41 accumulation" = "gdd41",
    "Daily GDD50 accumulation" = "gdd50",
    "Cumulative GDD41 since Jan 1" = "gdd41cum",
    "Cumulative GDD50 since Jan 1" = "gdd50cum",
    "Frost (<32F) this day" = "frost",
    "Hard freeze (<28F) this day" = "freeze"
  ),
  climate = list(
    "Mean daily max temp (F)" = "max_temp",
    "Mean daily min temp (F)" = "min_temp",
    "Mean daily GDD41" = "gdd41",
    "Mean daily GDD50" = "gdd50",
    "Mean cumulative GDD41" = "gdd41cum",
    "Mean cumulative GDD50" = "gdd50cum",
    "Mean probability of frost on day" = "frost",
    "Mean probability of hard freeze on day" = "freeze",
    "Cumulative probability of frost" = "frost_by",
    "Cumulative probability of hard freeze" = "freeze_by"
  ),
  comparison = list(
    "Max daily temp vs climate average (F)" = "max_temp",
    "Min daily temp vs climate average (F)" = "min_temp",
    "Daily GDD41 vs climate average" = "gdd41",
    "Daily GDD50 vs climate average" = "gdd50",
    "Cumul. GDD41 vs climate average" = "gdd41cum",
    "Cumul. GDD50 vs climate average" = "gdd50cum"
  )
)


# Functions ----

yesterday <- function() Sys.Date() - 1

make_date <- function(y, m, d) {
  as_date(paste(y, m, d, sep = "-"))
}

start_of_year <- function(d = Sys.Date()) {
  make_date(year(d), 1, 1)
}

end_of_year <- function(d = Sys.Date()) {
  make_date(year(d), 12, 31)
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

withSpinnerProxy <- function(ui, ...) {
  ui %>% shinycssloaders::withSpinner(type = 8, color = "#30a67d", ...)
}


# units: temp=F, pressure=kPa, rh=%
get_weather_grid <- function(date = yesterday()) {
  url <- str_glue("https://agweather.cals.wisc.edu/api/weather/grid?lat_range={MIN_LAT},{MAX_LAT}&long_range={MIN_LNG},{MAX_LNG}&date={date}")
  resp <- httr::GET(url) %>% httr::content()
  data <- resp$data %>%
    enframe() %>%
    unnest_wider("value")
  if (nrow(data) > 0) {
    data %>%
      fix_coords() %>%
      select(lat, lng, date, min_temp, max_temp, frost, freeze = freezing) %>%
      mutate(
        date = as_date(date),
        gdd41 = calc_gdd(min_temp, max_temp, 41, 86),
        gdd50 = calc_gdd(min_temp, max_temp, 50, 86)
      )
  } else {
    message(str_glue("Failed to retrieve weather data for {date}"))
    tibble()
  }
}

weather_dates <- function() {
  # message(str_glue("Checking weather data for {START_DATE} - {yesterday()}..."))
  dates_need <- sort(seq.Date(START_DATE, yesterday(), 1))
  dates_have <- if (exists("weather")) sort(unique(weather$date))
  as.character(dates_need[!(dates_need %in% dates_have)])
}

# downloads and saves missing daily weather from AgWeather
fill_weather <- function(dates = weather_dates()) {
  if (length(dates) == 0) {
    message("Everything up to date. Nothing to do.")
  } else {
    for (d in dates) {
      message(d, " ==> GET")
      wx <- get_weather_grid(d)
      if (!exists("weather")) {
        weather <<- wx
      } else {
        weather <<- bind_rows(weather, wx)
      }
    }
  }
  weather <<- weather %>%
    arrange(lat, lng, date) %>%
    mutate(year = year(date), .after = date) %>%
    mutate(
      gdd41cum = cumsum(gdd41),
      gdd50cum = cumsum(gdd50),
      .by = c(lat, lng, year)
    )
  weather %>% write_feather("data/weather.feather")
}


# Initialize data ----

counties <- read_rds("data/counties.rds")
if (!exists("climate")) climate <- read_rds("data/climate.rds")
if (file.exists("data/weather.feather")) weather <- read_feather("data/weather.feather")

# delete some weather for testing
# weather <- weather %>% filter(date < "2024-07-15")
# weather %>% write_feather("data/weather.feather")
