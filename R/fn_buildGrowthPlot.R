#' @param df data from `buildGrowthData`
#' @param loc list with lat, lng
#'
buildGrowthPlot <- function(df, loc) {
  # maximum of 180 days
  df <- head(df, 180)

  opts <- list()
  opts$cut_date <- min(df$date)
  opts$end_date <- max(df$date)
  opts$title <- sprintf(
    "Alfalfa growth projection from %s for %.1f°N, %.1f°W",
    format(opts$cut_date, "%b %d, %Y"), loc$lat, loc$lng
  )

  # cutting thresholds
  threshold_dates <- df %>%
    drop_na(growth_threshold) %>%
    distinct(growth_threshold, .keep_all = TRUE) %>%
    pull(date)

  # project at most 30 days beyond the last threshold
  if (length(threshold_dates) > 0 & max(df$gdd_since_kill) > max(OPTS$growth_thresholds)) {
    df <- df %>% filter(date <= max(threshold_dates) + 30)
  }

  # set gdd axis y range
  opts$yrange <- c(0, max(df$gdd_since_kill) * 1.1)

  # label any growth thresholds
  growth_annot <- df %>%
    filter(date %in% threshold_dates) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      round(gdd_since_kill), " GDD<br>",
      days_since_kill, " days"
    ))

  # label any killing freeze risks
  kill_annot <- df %>%
    filter(!is.na(kill_annot)) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      kill_annot, "<br>",
      days_since_kill, " days<br>",
      round(gdd_since_kill), " GDD"
    ))

  # base plot
  plt <- df %>%
    plot_ly() %>%
    add_trace(
      name = "Days since last kill or cut",
      x = ~date, y = ~days_since_kill,
      type = "scatter", mode = "none", hovertemplate = "%{y:.0f}",
      showlegend = F
    )

  # add observed hard freezes if any
  freezes <- df %>% filter(kill, source == "weather")
  if (nrow(freezes) > 0) {
    plt <- plt %>% add_trace(
      name = "Killing freeze (<24°F)",
      x = freezes$date, y = .025, yaxis = "y2",
      type = "bar", hovertemplate = "Yes",
      marker = list(color = "blue", line = list(opacity = 0)),
      width = 1000 * 60 * 60 * 24 # 1 day in ms
    )
  }

  # add projected hard freezes if any
  freezes <- df %>% filter(kill, source == "climate")
  if (nrow(freezes) > 0) {
    plt <- plt %>% add_trace(
      name = "Possible freeze (<24°F)",
      x = freezes$date, y = .025, yaxis = "y2",
      type = "bar", hovertemplate = "Yes",
      marker = list(color = "lightblue", line = list(opacity = 0)),
      width = 1000 * 60 * 60 * 24 # 1 day in ms
    )
  }

  # add gdd traces
  plt <- plt %>%
    add_trace(
      name = "Daily GDD<sub>41</sub>",
      x = ~date, y = ~gdd41, yaxis = "y1",
      type = "bar", hovertemplate = "%{y:.1f}",
      marker = list(color = "#00a038")
    ) %>%
    add_trace(
      name = "GDD<sub>41</sub> since last kill/cut",
      x = ~date, y = ~gdd_since_kill, yaxis = "y1",
      type = "scatter", mode = "lines", hovertemplate = "%{y:.1f}",
      line = list(color = "#00a038")
    )

  # add cumulative freeze probability
  if (max(df$kill_by) > 0) {
    plt <- plt %>%
      add_trace(
        name = "Cumul. killing freeze prob.",
        x = ~date, y = ~kill_by, yaxis = "y2",
        type = "scatter", mode = "lines", hovertemplate = "%{y:.0%}",
        line = list(color = "purple", width = 1.5)
      )
    opts$y2 <- list(
      title = "Cumul. killing freeze prob. (<24°F)",
      overlaying = "y",
      side = "right",
      zeroline = F,
      showgrid = F,
      fixedrange = T,
      tickformat = ".0%",
      range = c(0, 1.05)
    )
  }

  # add growth threshold annotations if any
  if (nrow(growth_annot) > 0) {
    plt <- plt %>%
      add_annotations(
        data = growth_annot,
        x = ~date, y = ~gdd_since_kill,
        text = ~label,
        ax = -25, ay = -35,
        arrowsize = .5,
        font = list(size = 10)
      )
  }

  # add killing freeze annotations if any
  if (nrow(kill_annot) > 0) {
    plt <- plt %>%
      add_annotations(
        data = kill_annot,
        x = ~date, y = ~ kill_by, yref = "y2",
        text = ~label,
        ax = 40,
        ay = 0,
        arrowsize = .5,
        font = list(size = 10)
      )
  }

  # update plot layout
  plt <- plt %>%
    layout(
      legend = OPTS$plot_legend,
      title = list(
        text = opts$title,
        yanchor = "bottom"
      ),
      xaxis = list(
        title = "Date",
        # dtick = 1000 * 60 * 60 * 24 * 7,
        ticks = "outside",
        showgrid = TRUE,
        gridwidth = .5,
        tickformat = "%b %d",
        hoverformat = "%b %d, %Y (day %j)",
        domain = c(0, .95)
      ),
      yaxis = list(
        title = "Growing degree days (base 41°F)",
        fixedrange = TRUE,
        gridwidth = .5,
        range = opts$yrange
      ),
      yaxis2 = opts$y2,
      hovermode = "x unified",
      margin = list(t = 50),
      modebar = list(remove = list("pan", "select", "lasso", "zoom", "autoscale"))
    ) %>%
    config(
      toImageButtonOptions = append(OPTS$plot_export_opts, list(filename = opts$title))
    )

  # add cut zone annotations
  cut_zones <- list(
    rect(800, 1200, color = "green"),
    rect(900, 1100, color = "green"),
    rect(0, 360, color = "blue")
  )

  # add line showing today if in range
  plt <- if (today() %in% df$date) {
    plt %>% add_today(other_shapes = cut_zones)
  } else {
    plt %>% layout(shapes = cut_zones)
  }

  # return the plot and the filtered data
  return(list(
    plt = plt,
    df = df
  ))
}
