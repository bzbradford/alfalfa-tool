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
    x = ~date, y = ~ if_else(min_temp <= 32, 32, NA),
    type = "scatter", mode = "lines",
    hovertemplate = "Yes",
    line = list(color = "orchid")
  ) %>%
  add_trace(
    name = "Hard freeze (<28F)",
    x = ~date, y = ~ if_else(min_temp <= 28, 28, NA),
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
    ~ zoo::rollapply(.x, width = 14, by = 1, mean, na.rm = T, partial = T)
  )) %>%
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
    x = ~date, y = ~ frost * 100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(color = "orchid", shape = "spline"),
    yaxis = "y2"
  ) %>%
  add_trace(
    name = "Hard freeze probability",
    x = ~date, y = ~ freeze * 100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(color = "purple", shape = "spline"),
    yaxis = "y2"
  ) %>%
  add_trace(
    name = "Cumulative freeze prob.",
    x = ~date, y = ~ freeze_by * 100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(color = "purple", shape = "spline"),
    yaxis = "y2"
  ) %>%
  add_trace(
    name = "GDD41 per day",
    x = ~date, y = ~gdd41,
    type = "scatter", mode = "lines",
    line = list(color = "green"),
    yaxis = "y3"
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
      domain = c(0, .9)
    ),
    yaxis = list(
      title = "Temperature (F)"
    ),
    yaxis2 = list(
      title = "Frost/freeze probability (%)",
      overlaying = "y",
      side = "right"
    ),
    yaxis3 = list(
      title = "Growing degree days",
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


plot_ly() %>%
  add_temp_traces(df, "y1")





# cut timing --------------------------------------------------------------

loc <- list(lat = 45, lng = -89)

wx <- weather %>%
  filter(lat == loc$lat, lng == loc$lng, year == 2024) %>%
  select(date, yday, gdd41) %>%
  mutate(source = "weather")

cl <- climate$c10 %>%
  filter(lat == loc$lat, lng == loc$lng) %>%
  filter(yday > max(wx$yday)) %>%
  select(yday, gdd41) %>%
  mutate(date = start_of_year(last(wx$date)) + yday - 1) %>%
  mutate(source = "projected")

cl_risk <- climate$c10 %>%
  filter(lat == loc$lat, lng == loc$lng) %>%
  select(yday, freeze_by)

bind_rows(wx, cl)

cut_points <- c(
  -1,
  yday(as_date(c("2024-6-12", "2024-7-21", "2024-8-30"))),
  366
)

df <- bind_rows(wx, cl) %>%
  left_join(cl_risk) %>%
  mutate(cutting = cut(yday, cut_points)) %>%
  mutate(gdd41cum = cumsum(gdd41)) %>%
  mutate(
    days_since_cut = n(),
    gdd_since_cut = cumsum(gdd41),
    .by = cutting
  )

df %>%
  plot_ly() %>%
  add_trace(
    name = "GDD41 since Jan 1",
    x = ~date, y = ~gdd41cum,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}",
    line = list(dash = "dot"),
    yaxis = "y1"
  ) %>%
  add_trace(
    name = "GDD41 since last cutting",
    x = ~date, y = ~gdd_since_cut,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}",
    # line = list(color = ~cutting),
    yaxis = "y1"
  ) %>%
  add_trace(
    name = "Hard freeze prob.",
    x = ~date, y = ~ freeze_by * 100,
    type = "scatter", mode = "lines",
    hovertemplate = "%{y:.1f}%",
    line = list(
      color = "purple",
      shape = "spline",
      width = 1.5
    ),
    yaxis = "y2"
  ) %>%
  layout(
    title = "Weather",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Growing degree days (base 41F)"),
    yaxis2 = list(
      title = "Cumulative hard freeze probability",
      overlaying = "y",
      side = "right"
    ),
    hovermode = "x unified",
    shapes = list(
      rect(900, 1100, color = "green"),
      rect(0, 360, color = "blue"),
      vline()
    )
  )


# find gdd closest to 500

df[which.min(abs(1000 - df$gdd41cum)), ]




# Growth plot -------------------------------------------------------------

test_loc <- list(lat = 45, lng = -89)
test_wx <- weather %>% filter(lat == test_loc$lat, lng == test_loc$lng)
test_cl <- climate$c10 %>% filter(lat == test_loc$lat, lng == test_loc$lng)

buildGrowthPlotData(test_wx, test_cl, as_date("2024-4-15"))
test_data <- buildGrowthData(test_wx, test_cl, as_date("2025-1-1"))

buildGrowthPlot(test_data, test_loc)



cut_dates <- c("2025-4-1", "2025-6-1", "2025-8-1") %>% as_date()
req(identical(cut_dates, sort(unique(cut_dates))))
cut_days <- yday(cut_dates)
cut_points <- c(-1, cut_days, 999)
df <- test_data %>%
  mutate(cutting = cut(yday, cut_points)) %>%
  mutate(
    days_since_cut = row_number() - 1,
    gdd_since_cut = cumsum(gdd41),
    .by = c(last_kill, cutting)
  )


buildTimingPlot(test_data, test_loc, 2025, cut_dates)
