#' @param df data from `buildGrowthData`
#' @param loc list with lat, lng
#'
buildGrowthPlot <- function(df, loc) {
  opts <- list()
  opts$cut_date <- min(df$date)
  opts$end_date <- max(df$date)
  opts$title <- sprintf(
    "Alfalfa growth projection since %s for %.1f°N, %.1f°W",
    format(opts$cut_date, "%b %d, %Y"), loc$lat, loc$lng
  )

  # cutting thresholds
  thresholds <- seq(800, 1200, by = 100)
  thresholds <- thresholds[thresholds < last(df$gdd_since_kill)]
  threshold_dates <- sapply(thresholds, function(gdd) {
    df[which.min(abs(gdd - df$gdd_since_kill)), ]$date
  })
  if (length(threshold_dates) > 0) {
    threshold_dates <- as_date(threshold_dates)
    df <- df %>% filter(date <= max(threshold_dates) + 30)
  }
  opts$yrange <- c(0, max(df$gdd_since_kill) * 1.1)

  cut_annot <- df %>%
    filter(date %in% threshold_dates) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      round(gdd_since_kill), " GDD<br>",
      days_since_kill, " days"
    ))

  # identify fall kill probability to annotate the 50% date
  kill_date <- df %>%
    filter((lag(kill_by) < .5 & kill_by >= .5) | (kill_by <= .5 & lead(kill_by) > .5)) %>%
    slice_min(abs(kill_by - .5)) %>%
    pull(date)

  # annotate fall kill probability
  kill_annot <- df %>%
    filter(date %in% kill_date) %>%
    filter(yday > 200) %>%
    mutate(label = paste0(
      "<b>", format(date, "%b %d"), "</b><br>",
      "50% kill<br>probability<br>",
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

  # add gdd traces
  plt <- plt %>%
    add_trace(
      name = "GDD41",
      x = ~date, y = ~gdd41, yaxis = "y1",
      type = "bar", hovertemplate = "%{y:.1f}",
      marker = list(color = "#00a038")
    ) %>%
    add_trace(
      name = "GDD41 since last kill/cut",
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
      range = c(0, 1)
    )
  }

  # add cut annotation if any
  if (nrow(cut_annot) > 0) {
    plt <- plt %>%
      add_annotations(
        data = cut_annot,
        x = ~date, y = ~gdd_since_kill,
        text = ~label,
        ax = -25, ay = -35,
        arrowsize = .5,
        font = list(size = 10)
      )
  }

  # add kill annotation if any
  if (nrow(kill_annot) > 0) {
    plt <- plt %>%
      add_annotations(
        data = kill_annot,
        x = ~date, y = ~ kill_by * 100, yref = "y2",
        text = ~label,
        ax = 40, ay = 30,
        arrowsize = .5,
        font = list(size = 10)
      )
  }

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

  cut_zones <- list(
    rect(800, 1200, color = "green"),
    rect(900, 1100, color = "green"),
    rect(0, 360, color = "blue")
  )

  if (today() %in% df$date) {
    plt %>% add_today(other_shapes = cut_zones)
  } else {
    plt %>% layout(shapes = cut_zones)
  }
}
