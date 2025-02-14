#' @param df data from `buildGrowthData`
#' @param loc list with lat, lng
#' @param year scheduling year
#'
buildTimingPlot <- function(df, loc, weather_year, cut_dates) {
  opts <- list()
  opts$title <- sprintf("%s Alfalfa cutting schedule for %.1f°N, %.1f°W", weather_year, loc$lat, loc$lng)

  req(identical(cut_dates, sort(unique(cut_dates))))
  cut_days <- yday(cut_dates)
  cut_points <- c(-1, cut_days, 999)
  df <- df %>%
    filter(year(date) == weather_year, yday > 31) %>%
    mutate(cutting = cut(yday, cut_points)) %>%
    mutate(
      days_since_cut = row_number() - 1,
      gdd_since_cut = cumsum(gdd41),
      .by = c(last_kill, cutting)
    )

  killing_freezes <- df %>% filter(kill)

  # cutting date annotations
  cut_annot <- df %>%
    summarize(
      across(c(date, days_since_cut, gdd_since_cut), max),
      .by = cutting
    ) %>%
    head(-1) %>%
    select(-cutting) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      days_since_cut, " days<br>",
      round(gdd_since_cut), " GDD"
    ))

  # find last spring kill date
  last_spring_kill <- df %>%
    filter(kill, yday < 150) %>%
    tail(1) %>%
    mutate(label = paste0("<b>", format(date, "%b %d"), "</b><br>Last spring kill"))

  # find first fall killing freeze
  first_fall_kill <- df %>%
    mutate(across(c(days_since_cut, gdd_since_cut), lag)) %>%
    filter(kill, yday > 150) %>%
    head(1) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      "First fall kill<br>",
      days_since_cut, " days<br>",
      round(gdd_since_cut), " GDD"
    ))

  # if there has been no fall killing freeze, find the 50% likelihood date
  if (nrow(first_fall_kill) == 0) {
    dt <- df %>%
      filter(yday > 200) %>%
      slice_min(abs(kill_by - .5)) %>%
      pull(date)
    first_fall_kill <- df %>%
      filter(date == first(dt)) %>%
      mutate(label = paste0(
        "<b>", format(date, "%b %d"), "</b><br>",
        "50% kill<br>probability<br>",
        days_since_cut, " days<br>",
        round(gdd_since_cut), " GDD"
      ))
  }

  kill_annot <- bind_rows(last_spring_kill, first_fall_kill)

  plt <- plot_ly(df) %>%
    add_trace(
      name = "Days since last kill or cut",
      x = ~date, y = ~days_since_cut,
      type = "scatter", mode = "none", hovertemplate = "%{y:.0f}",
      showlegend = F
    )

  if (nrow(killing_freezes) > 0) {
    plt <- plt %>% add_trace(
      name = "Killing freeze (<24°F)",
      x = killing_freezes$date, y = 200,
      type = "bar", hovertemplate = "Yes",
      marker = list(color = "blue"), width = 1000 * 60 * 60 * 24
    )
  }

  plt <- plt %>%
    add_trace(
      name = "Cumul. GDD41 (climate average)",
      x = ~date, y = ~gdd41cum_cl, yaxis = "y1",
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = "#ad2b2f", shape = "spline")
    ) %>%
    add_trace(
      name = "Cumul. GDD41 (observed/projected)",
      x = ~date, y = ~gdd41cum, yaxis = "y1",
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = "#ff802e", shape = "spline", dash = "dot")
    ) %>%
    add_trace(
      name = "GDD41 since last kill/cut",
      x = ~date, y = ~gdd_since_cut, yaxis = "y1",
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = "#00a038")
    ) %>%
    add_trace(
      name = "Cumul. killing freeze prob.",
      x = ~date, y = ~ kill_by * 100, yaxis = "y2",
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}%",
      line = list(color = "purple", width = 1.5)
    ) %>%
    add_annotations(
      data = cut_annot,
      x = ~date, y = ~gdd_since_cut, text = ~label,
      arrowsize = .5,
      font = list(size = 10)
    ) %>%
    add_annotations(
      data = kill_annot,
      x = ~date, y = ~gdd_since_cut, text = ~label,
      arrowsize = .5,
      font = list(size = 10)
    ) %>%
    layout(
      legend = OPTS$plot_legend,
      title = list(
        text = opts$title,
        yanchor = "bottom"
      ),
      xaxis = OPTS$plot_date_axis_weather,
      yaxis = list(
        title = "Growing degree days (base 41°F)",
        fixedrange = T,
        range = c(0, 5000)
      ),
      yaxis2 = list(
        title = "Cumul. killing freeze prob. (<24°F)",
        overlaying = "y",
        side = "right",
        zeroline = F,
        showgrid = F,
        fixedrange = T,
        range = c(0, 100)
      ),
      hovermode = "x unified",
      margin = list(t = 50),
      modebar = list(
        remove = list("pan", "select", "lasso", "zoom", "autoscale")
      )
    ) %>%
    config(
      toImageButtonOptions = append(OPTS$plot_export_opts, list(filename = opts$title))
    )

  cut_zones <- list(
    rect(800, 1200, color = "green"),
    rect(900, 1100, color = "green"),
    rect(0, 360, color = "blue")
  )

  plt %>% add_today(yr = weather_year, date_yr = weather_year, other_shapes = cut_zones)
}
