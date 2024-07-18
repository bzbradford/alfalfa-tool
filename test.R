

# weather plot

loc <- list(lat = 45, lng = -89)

df <- weather %>%
  filter(lat == loc$lat, lng == loc$lng) %>%
  mutate(min_temp = zoo::rollapply(min_temp, width = 14, by = 1, mean, na.rm = T, partial = T))

df %>%
  plot_ly() %>%
  add_trace(
    name = "Min temp",
    x = ~date, y = ~min_temp,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}°F",
    yaxis = "y1",
    line = list(color = "blue", shape = "spline")
  ) %>%
  add_trace(
    name = "Mean temp",
    x = ~date, y = ~mean_temp,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}°F",
    yaxis = "y1",
    line = list(color = "orange", shape = "spline")
  ) %>%
  add_trace(
    name = "Max temp",
    x = ~date, y = ~max_temp,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}°F",
    yaxis = "y1",
    line = list(color = "red", shape = "spline")
  ) %>%
  add_trace(
    name = "Frost (<32F)",
    x = ~date, y = ~if_else(min_temp <= 32, 32, NA),
    type = "scatter", mode = "line",
    hovertemplate = "Yes",
    line = list(color = "orchid")
  ) %>%
  add_trace(
    name = "Hard freeze (<28F)",
    x = ~date, y = ~if_else(min_temp <= 28, 28, NA),
    type = "scatter", mode = "lines",
    hovertemplate = "Yes",
    line = list(color = "purple")
  ) %>%
  layout(
    title = "Weather",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Temperature (F)"),
    yaxis2 = list(
      title = "Daily GDD",
      overlaying = "y",
      side = "right"
    ),
    yaxis3 = list(
      title = "Cumulative GDD",
      overlaying = "y",
      side = "right",
      position = 1
    ),
    yaxis4 = list(
      title = "Frost/Freeze",
      overlaying = "y",
      side = "left",
      position = 0
    ),
    hovermode = "x unified"
  )
