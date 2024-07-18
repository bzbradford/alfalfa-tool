

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
    type = "scatter", mode = "lines",
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
  add_trace(
    name = "GDD41 since Jan 1",
    x = ~date, y = ~gdd41cum,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}",
    line = list(color = "green"),
    yaxis = "y2"
  ) %>%
  add_trace(
    name = "GDD50 since Jan 1",
    x = ~date, y = ~gdd50cum,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}",
    line = list(color = "brown"),
    yaxis = "y2"
  ) %>%
  layout(
    title = "Weather",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Temperature (F)"),
    yaxis2 = list(
      title = "Growing degree days",
      overlaying = "y",
      side = "right",
      position = 1
    ),
    hovermode = "x unified"
  )



# climate plot

loc <- list(lat = 45, lng = -89)

df <- climate$c10 %>%
  filter(lat == loc$lat, lng == loc$lng) %>%
  mutate(across(
    c(frost, freeze),
    ~zoo::rollapply(.x, width = 14, by = 1, mean, na.rm = T, partial = T))) %>%
  mutate(date = start_of_year() + yday - 1)

df %>%
  plot_ly() %>%
  add_trace(
    name = "Avg Min temp",
    x = ~date, y = ~min_temp,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}°F",
    yaxis = "y1",
    line = list(color = "cornflowerblue", shape = "spline")
  ) %>%
  add_trace(
    name = "Avg Mean temp",
    x = ~date, y = ~mean_temp,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}°F",
    yaxis = "y1",
    line = list(color = "orange", shape = "spline")
  ) %>%
  add_trace(
    name = "Avg Max temp",
    x = ~date, y = ~max_temp,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}°F",
    yaxis = "y1",
    line = list(color = "#c5050c", shape = "spline")
  ) %>%
  add_trace(
    name = "Frost probability",
    x = ~date, y = ~frost*100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(color = "orchid", shape = "spline"),
    yaxis = "y2"
  ) %>%
  add_trace(
    name = "Hard freeze probability",
    x = ~date, y = ~freeze*100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(color = "purple", shape = "spline"),
    yaxis = "y2"
  ) %>%
  add_trace(
    name = "Cumulative freeze prob.",
    x = ~date, y = ~freeze_by*100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(color = "purple", shape = "spline"),
    yaxis = "y2"
  ) %>%
  layout(
    title = "Weather",
    hovermode = "x unified",
    xaxis = list(
      title = "Date",
      dtick = "M1",
      tickformat = "%b",
      hoverformat = "%b %d",
      automargin = T,
      domain = c(0, .9)),
    yaxis = list(
      title = "Temperature (F)"),
    yaxis2 = list(
      title = "Frost/freeze probability (%)",
      overlaying = "y",
      side = "right"
    ),
    shapes = list(
      list(
        type = "line",
        x0 = Sys.Date(), x1 = Sys.Date(),
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "black", dash = "dot"),
        opacity = .5
      )
    )
  )

