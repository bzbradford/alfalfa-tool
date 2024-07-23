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
    "Mean daily temp (F)" = "mean_temp",
    "Min daily temp (F)" = "min_temp",
    "Max daily temp (F)" = "max_temp",
    "Daily GDD41 accumulation" = "gdd41",
    "Daily GDD50 accumulation" = "gdd50",
    "Cumulative GDD41 since Jan 1" = "gdd41cum",
    "Cumulative GDD50 since Jan 1" = "gdd50cum"
    # "Frost (<32F) this day" = "frost",
    # "Hard freeze (<28F) this day" = "freeze"
  ),
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
    "Cumulative probability of hard freeze" = "freeze_by"
  ),
  comparison = list(
    "Mean daily temp vs climate average (F)" = "mean_temp",
    "Min daily temp vs climate average (F)" = "min_temp",
    "Max daily temp vs climate average (F)" = "max_temp",
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

add_weather_cols <- function(.data) {
  .data %>%
    arrange(lat, lng, date) %>%
    mutate(
      year = year(date),
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
get_weather_grid <- function(date = yesterday()) {
  url <- str_glue("https://agweather.cals.wisc.edu/api/weather/grid?lat_range={MIN_LAT},{MAX_LAT}&long_range={MIN_LNG},{MAX_LNG}&date={date}")
  resp <- httr::GET(url) %>% httr::content()
  data <- resp$data %>%
    enframe() %>%
    unnest_wider("value")
  if (nrow(data) > 0) {
    data %>%
      fix_coords() %>%
      select(lat, lng, date, min_temp, max_temp) %>%
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

  # save minimal dataset
  weather %>% write_feather("data/weather.feather")

  # build additional cols
  weather <<- weather %>% add_weather_cols()
}


## UI builders ----

add_climate_period_ui <- function(id) {
  radioButtons(
    inputId = id,
    label = "Climate period",
    choices = list(
      "10-year climate average (2013-2023)" = "c10",
      "5-year climate average (2018-2023)" = "c5"
    )
  )
}

add_smoothing_ui <- function(id, inline = TRUE) {
  radioButtons(
    inputId = id,
    label = "Data smoothing options",
    choices = list(
      "Daily observations (no smoothing)" = 1,
      "Weekly rolling mean" = 7,
      "14-day rolling mean" = 14
    ),
    inline = inline
  )
}


## Plot helpers ----

set_axis <- function(plt, yaxis, title) {
  axis <-  list(
    title = title,
    zeroline = F
  )
  if (yaxis == "y1") {
    plt %>% layout(yaxis = axis)
  } else {
    axis$overlaying = "y"
    axis$side = "right"
    plt %>% layout(yaxis2 = axis)
  }
}

add_temp_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- 1

  plt %>%
    add_trace(
      name = paste0(label, "Min temp"),
      x = df$date, y = df$min_temp,
      type = "scatter", mode = "lines",
      line = list(
        color = "cornflowerblue",
        width = width,
        shape = "spline",
        dash = dash),
      hovertemplate = "%{y:.1f}°F",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Mean temp"),
      x = df$date, y = df$mean_temp,
      type = "scatter", mode = "lines",
      line = list(
        color = "orange",
        width = width,
        shape = "spline",
        dash = dash),
      hovertemplate = "%{y:.1f}°F",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Max temp"),
      x = df$date, y = df$max_temp,
      type = "scatter", mode = "lines",
      line = list(
        color = "#c5050c",
        width = width,
        shape = "spline",
        dash = dash),
      hovertemplate = "%{y:.1f}°F",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Frost (<32F)"),
      x = df$date, y = if_else(df$min_temp <= 32, 32, NA),
      type = "scatter", mode = "lines",
      line = list(
        color = "orchid",
        width = width,
        dash = dash),
      hovertemplate = "Yes",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "Hard freeze (<28F)"),
      x = df$date, y = if_else(df$min_temp <= 28, 28, NA),
      type = "scatter", mode = "lines",
      line = list(
        color = "purple",
        width = width,
        dash = dash),
      hovertemplate = "Yes",
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "Temperature (F)")
}

add_gdd_daily_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- 1

  plt %>%
    add_trace(
      name = paste(label, "GDD41/day"),
      x = df$date, y = df$gdd41,
      type = "scatter", mode = "lines",
      line = list(
        color = "green",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste(label, "GDD50/day"),
      x = df$date, y = df$gdd50,
      type = "scatter", mode = "lines",
      line = list(
        color = "peru",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "GDD accumulation/day")
}

add_gdd_cum_traces <- function(plt, df, yaxis = "y1", label = "", dash = F) {
  dash <- ifelse(dash, "dashdot", "none")
  width <- 1

  plt %>%
    add_trace(
      name = paste0(label, "GDD41 (cumul.)"),
      x = df$date, y = df$gdd41cum,
      type = "scatter", mode = "lines",
      line = list(
        color = "olivedrab",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = paste0(label, "GDD50 (cumul.)"),
      x = df$date, y = df$gdd50cum,
      type = "scatter", mode = "lines",
      line = list(
        color = "chocolate",
        width = width,
        dash = dash),
      hovertemplate = "%{y:.1f}",
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "Cumulative GDD")
}

add_frost_traces <- function(plt, df, yaxis = "y1") {
  width <- 1

  plt %>%
    add_trace(
      name = "Frost probability",
      x = df$date, y = df$frost*100,
      type = "scatter", mode = "lines",
      line = list(
        color = "orchid",
        width = width,
        shape = "spline"),
      hovertemplate = "%{y:.1f}%",
      yaxis = yaxis
    ) %>%
    add_trace(
      name = "Hard freeze probability",
      x = df$date, y = df$freeze*100,
      type = "scatter", mode = "lines",
      hovertemplate = "%{y:.1f}%",
      line = list(
        color = "purple",
        width = width,
        shape = "spline"),
      yaxis = yaxis
    ) %>%
    add_trace(
      name = "Cumul. frost prob.",
      x = df$date, y = df$frost_by*100,
      type = "scatter", mode = "lines",
      hovertemplate = "%{y:.1f}%",
      line = list(
        color = "orchid",
        width = width,
        shape = "spline",
        dash = "dot"),
      yaxis = yaxis
    ) %>%
    add_trace(
      name = "Cumul. freeze prob.",
      x = df$date, y = df$freeze_by*100,
      type = "scatter", mode = "lines",
      hovertemplate = "%{y:.1f}%",
      line = list(
        color = "purple",
        width = width,
        shape = "spline",
        dash = "dot"),
      yaxis = yaxis
    ) %>%
    set_axis(yaxis, "Frost/freeze probability")
}


# Initialize data ----

counties <- read_rds("data/counties.rds")

if (!exists("climate")) {
  climate <- read_rds("data/climate.rds") %>% lapply(add_climate_cols)
}

if (file.exists("data/weather.feather")) {
  weather <- read_feather("data/weather.feather") %>%
    add_weather_cols()
}

# delete some weather for testing
# weather <- weather %>% filter(date < "2024-07-15")
# weather %>% write_feather("data/weather.feather")
