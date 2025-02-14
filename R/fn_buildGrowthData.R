#' Combine weather and climate data to project alfalfa growth
#' @param weather_data weather data for a single location
#' @param climate_data climate data for a single location
#' @param start_date start of growth projection
#'
buildGrowthData <- function(weather_data, climate_data, start_date) {
  wx <- weather_data %>%
    filter(date >= start_date) %>%
    select(date, gdd41, kill) %>%
    mutate(source = "weather")

  cl <- climate_data %>%
    select(yday, gdd41_cl = gdd41, kill_by)

  tibble(date = seq.Date(start_date, start_date + 365, 1), yday = yday(date)) %>%
    left_join(wx, join_by(date)) %>%
    left_join(cl, join_by(yday)) %>%
    mutate(
      gdd41 = coalesce(gdd41, gdd41_cl),
      gdd41cum = cumsum(gdd41),
      gdd41cum_cl = cumsum(gdd41_cl)
    ) %>%
    replace_na(list(source = "climate", kill = F)) %>%
    mutate(last_kill = if_else(kill | row_number() == 1, date, NA)) %>%
    fill(last_kill) %>%
    mutate(gdd41 = if_else(kill, 0, gdd41)) %>%
    mutate(
      gdd_since_kill = cumsum(gdd41),
      days_since_kill = as.integer(date - last_kill),
      .by = last_kill
    )
}
