

# weather plot

loc <- list(lat = 45, lng = -89)
weather %>%
  filter(lat == loc$lat, lng == loc$lng) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    name = "Min temperature (F)",
    x = ~date,
    y = ~min_temp
  ) %>%
  add_trace(
    name = "Max temperature (F)",
    x = ~date,
    y = ~max_temp,

  )
