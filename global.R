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
  library(shinyWidgets) # radioGroupButtons
  library(htmltools) # tagList
  library(shinycssloaders) # withSpinner
  # library(DT) # data tables
  library(leaflet) # map
  library(leaflet.extras) # map JS buttons
  library(plotly) # plots
})



# Utility functions ----

# for agweather
yesterday <- function() {
  as.Date(format(Sys.time() - 60 * 60, tz = "America/Chicago")) - 1
}

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

google_key <- Sys.getenv("google_places_key")
# style <- read_file("www/style.css") %>% str_replace_all("[\r\n]", " ")
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
  # weather_date_fmt = "%b %-d, %Y",
  weather_date_fmt = "%b %-d",
  climate_date_fmt = "%b %-d",
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
    "10-year average (2013-2023)" = "c10",
    "5-year average (2018-2023)" = "c5"
  ),
  plot_period_prefix = list(
    "c10" = "10-year average (2013-2023)",
    "c5" = "5-year average (2018-2023)"
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
  plot_smoothing_prefix = list(
    "1" = "Daily",
    "7" = "7-day average",
    "14" = "14-day average",
    "28" = "28-day average"
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
  location_validation_msg = "Please select a grid cell on the map first. Use the crosshair icon on the map to automatically select your location, or enter a place name in the searchbox below the map.",

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

in_extent <- function(lat, lng) {
  between(lat, OPTS$min_lat, OPTS$max_lat) &
    between(lng, OPTS$min_lng, OPTS$max_lng)
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

smooth_cols <- function(.data, width, cols = OPTS$smoothable_cols) {
  width <- as.numeric(width)
  if (width == 1) return(.data)
  mutate(.data, across(
    any_of(cols),
    ~zoo::rollapply(.x, width = width, FUN = mean, na.rm = T, partial = T)
  ))
}

remove_weather_cols <- function(.data) {
  drop_cols <- c("year", "yday", "mean_temp", "frost", "freeze", "kill", "gdd41cum", "gdd50cum")
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
      freeze = min_temp <= 28,
      kill = min_temp <= 24
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
    message(str_glue("Failed to retrieve weather data for {d}"))
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
